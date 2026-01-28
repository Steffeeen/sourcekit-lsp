//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

@_spi(SourceKitLSP) package import LanguageServerProtocol
import SourceKitLSP
import SwiftOperators
import SwiftSyntax

extension SwiftLanguageService {
  package func selectionRange(_ req: SelectionRangeRequest) async throws -> [SelectionRange] {
    let snapshot = try self.documentManager.latestSnapshot(req.textDocument.uri)
    let sourceFile = await syntaxTreeManager.syntaxTree(for: snapshot)

    try Task.checkCancellation()

    return req.positions.map { position in
      let absolutePosition = snapshot.absolutePosition(of: position)

      guard let token = findIntuitiveToken(in: sourceFile, at: absolutePosition), let initialNode = token.parent else {
        return SelectionRange(range: position..<position)
      }

      if let selectionRange = computeSelectionRangeFor(
        position: absolutePosition,
        snapshot: snapshot,
        initialNode: initialNode,
        token: token
      ) {
        return selectionRange
      } else {
        return SelectionRange(range: position..<position)
      }
    }
  }
}

private func findIntuitiveToken(in sourceFile: SourceFileSyntax, at position: AbsolutePosition) -> TokenSyntax? {
  guard let currentToken = sourceFile.token(at: position) else {
    return nil
  }

  let boundaryTokens: [TokenKind] = [
    .rightParen, .rightBrace, .rightSquare, .comma, .semicolon, .period, .colon, .rightAngle,
  ]

  if position == currentToken.position && boundaryTokens.contains(currentToken.tokenKind) {
    // the cursor is at the start of a boundary token (e.g. `test(a: 3, b: 2|)`)
    // here the user most likely wants to select the `2` and then `b: 2` instead of
    // selecting the entire function call, so we use the previous token
    let newToken = currentToken.previousToken(viewMode: .sourceAccurate) ?? currentToken
    return newToken
  }

  return currentToken
}

private func computeSelectionRangeFor(
  position: AbsolutePosition,
  snapshot: DocumentSnapshot,
  initialNode: Syntax,
  token: TokenSyntax
) -> SelectionRange? {
  var ranges: [Range<AbsolutePosition>] = []

  var child = Syntax(token)
  while let parent = child.parent {
    let rangesForNode = calculateRangesFor(node: parent, previousNode: child, snapshot: snapshot, position: position)

    for range in rangesForNode {
      if ranges.last == range {
        // some ast nodes have the exact same range, we just skip creating ranges for them
        continue
      }

      ranges.append(range)
    }

    child = parent
  }

  var selectionRange: SelectionRange? = nil
  for range in ranges.reversed() {
    let start = snapshot.position(of: range.lowerBound)
    let end = snapshot.position(of: range.upperBound)
    selectionRange = SelectionRange(range: start..<end, parent: selectionRange)
  }

  return selectionRange
}

private func calculateRangesFor(
  node: Syntax,
  previousNode: Syntax,
  snapshot: DocumentSnapshot,
  position: AbsolutePosition
) -> [Range<AbsolutePosition>] {
  switch node.as(SyntaxEnum.self) {

  case .classDecl(let classDeclaration):
    return calculateRangesForDeclaration(
      previousNode: previousNode,
      declaration: DeclSyntax(classDeclaration),
      nameOrType: Syntax(classDeclaration.name),
      genericParameters: classDeclaration.genericParameterClause
    )

  case .structDecl(let structDeclaration):
    return calculateRangesForDeclaration(
      previousNode: previousNode,
      declaration: DeclSyntax(structDeclaration),
      nameOrType: Syntax(structDeclaration.name),
      genericParameters: structDeclaration.genericParameterClause
    )

  case .protocolDecl(let protocolDeclaration):
    return calculateRangesForDeclaration(
      previousNode: previousNode,
      declaration: DeclSyntax(protocolDeclaration),
      nameOrType: Syntax(protocolDeclaration.name),
      genericParameters: nil
    )

  case .extensionDecl(let extensionDeclaration):
    return calculateRangesForDeclaration(
      previousNode: previousNode,
      declaration: DeclSyntax(extensionDeclaration),
      nameOrType: Syntax(extensionDeclaration.extendedType),
      genericParameters: nil
    )

  case .stringSegment(let segment):
    return calculateRangesInside(stringSegment: segment, snapshot: snapshot, position: position)

  case .functionCallExpr(let functionCall):
    return calculateRangesInside(functionCall: functionCall, previousNode: previousNode)

  case .subscriptCallExpr(let subscriptCall):
    return calculateRangesInside(subscriptCall: subscriptCall, previousNode: previousNode)

  case .labeledExpr(let labeledExpression):
    return calculateRangesInside(labeledExpression: labeledExpression, previousNode: previousNode)

  case .functionDecl(let functionDeclaration):
    return calculateRangesInside(functionDeclaration: functionDeclaration, previousNode: previousNode)

  case .genericParameterClause(let genericParameterClause):
    return calculateRangesInside(genericParameterClause: genericParameterClause, previousNode: previousNode)

  case .genericParameter(let genericParameter):
    return calculateRangesInside(genericParameter: genericParameter)

  case .functionParameter(let parameter):
    return calculateRangesInside(parameter: parameter, previousNode: previousNode)

  case .functionEffectSpecifiers(let effectSpecifiers):
    return calculateRangesInside(effectSpecifiers: effectSpecifiers, previousNode: previousNode)

  case .closureSignature(let closureSignature):
    return calculateRangesInside(closureSignature: closureSignature)

  case .enumCaseParameter(let enumParameter):
    return calculateRangesInside(enumParameter: enumParameter, previousNode: previousNode)

  case .exprList(let expressionList):
    return calculateRangesInside(expressionList: expressionList, previousNode: previousNode)

  case .patternBinding(let patternBinding):
    return calculateRangesInside(patternBinding: patternBinding)

  case .codeBlock(let codeBlock):
    return calculateRangesInside(codeBlock: codeBlock)

  case .forStmt(let forStatement):
    return calculateRangesInside(forStatement: forStatement)

  case .associatedTypeDecl(let associatedTypeDeclaration):
    return calculateRangesInside(associatedTypeDeclaration: associatedTypeDeclaration, previousNode: previousNode)

  case .typeAliasDecl(let typeAliasDeclaration):
    return calculateRangesInside(typeAliasDeclaration: typeAliasDeclaration, previousNode: previousNode)

  case .dictionaryElement(let dictionaryElement):
    return calculateRangesInside(dictionaryElement: dictionaryElement)

  case .operatorDecl(let operatorDeclaration):
    return calculateRangesInside(operatorDeclaration: operatorDeclaration, previousNode: previousNode)

  case .memberAccessExpr(let memberAccess):
    return calculateRangesInside(memberAccess: memberAccess)

  case .identifierType(let identifierType):
    return calculateRangesInside(identifierType: identifierType)

  case .availabilityArgument(let availabilityArgument):
    return calculateRangesInside(availabilityArgument: availabilityArgument)

  case .patternBindingList, .initializerClause, .matchingPatternCondition, .sequenceExpr,
    .accessorDeclList, .functionParameterClause, .functionSignature, .switchCaseLabel, .switchCaseList,
    .inheritedType, .memberBlockItemList, .memberBlock, .enumCaseParameterClause,
    .optionalChainingExpr, .tuplePatternElement, .arrayElement, .keyPathComponent, .keyPathComponentList:
    return []

  default:
    return [node.trimmedRange]
  }
}

private func calculateRangesForDeclaration(
  previousNode: Syntax,
  declaration: DeclSyntax,
  nameOrType: Syntax,
  genericParameters: GenericParameterClauseSyntax?,
) -> [Range<AbsolutePosition>] {
  var ranges: [Range<AbsolutePosition>] = []

  if previousNode.id == nameOrType.id {
    ranges.append(nameOrType.trimmedRange)
  } else if let genericParameters = genericParameters,
    previousNode.id == genericParameters.id
  {
    let start = nameOrType.positionAfterSkippingLeadingTrivia
    let end = genericParameters.endPositionBeforeTrailingTrivia
    ranges.append(start..<end)
  }

  ranges.append(declaration.trimmedRange)

  return ranges
}

private func calculateRangesInside(
  stringSegment: StringSegmentSyntax,
  snapshot: DocumentSnapshot,
  position: AbsolutePosition
) -> [Range<AbsolutePosition>] {
  let offsetInString = position.utf8Offset - stringSegment.positionAfterSkippingLeadingTrivia.utf8Offset

  let text = stringSegment.content.text
  let index = text.index(text.startIndex, offsetBy: offsetInString)

  if text[index].isWhitespace {
    return []
  }

  var start = index
  while text.startIndex < start {
    let prev = text.index(before: start)
    if text[prev].isWhitespace {
      break
    }
    start = prev
  }

  var end = index
  while end < text.endIndex {
    if text[end].isWhitespace {
      break
    }
    end = text.index(after: end)
  }

  let startOffsetInString = text.distance(from: text.startIndex, to: start)
  let endOffsetInString = text.distance(from: text.startIndex, to: end)

  let startPosition = stringSegment.positionAfterSkippingLeadingTrivia.advanced(by: startOffsetInString)
  let endPosition = stringSegment.positionAfterSkippingLeadingTrivia.advanced(by: endOffsetInString)

  return [startPosition..<endPosition]
}

private func calculateRangesInside(
  functionCall: FunctionCallExprSyntax,
  previousNode: Syntax,
) -> [Range<AbsolutePosition>] {
  if let memberAccess = functionCall.calledExpression.as(MemberAccessExprSyntax.self),
    functionCall.parent?.as(ExpressionPatternSyntax.self) == nil,
    previousNode.id == functionCall.arguments.id
      || previousNode.id == functionCall.trailingClosure?.id
  {
    // Special case for adding an extra range including the function name and parameters/trailing closures
    // this is needed for chained method calls
    // Example:
    // numbers
    //  .filter { $0 > 0 }
    //  .map { $0 * 2 }
    //  .reduce(0,| +)
    //
    // when starting a selection from | we want to have a selection for `reduce(0, +)` in addition to selecting
    // the entire function call (starting from `numbers`)
    return [
      memberAccess.declName.positionAfterSkippingLeadingTrivia..<functionCall.endPositionBeforeTrailingTrivia,
      functionCall.trimmedRange,
    ]
  }

  // the default case: just create a range for the function call node
  return [functionCall.trimmedRange]
}

private func calculateRangesInside(
  subscriptCall: SubscriptCallExprSyntax,
  previousNode: Syntax,
) -> [Range<AbsolutePosition>] {
  if previousNode.id == subscriptCall.arguments.id {
    let start = subscriptCall.leftSquare.positionAfterSkippingLeadingTrivia
    let end = subscriptCall.rightSquare.endPositionBeforeTrailingTrivia
    return [start..<end, subscriptCall.trimmedRange]
  }

  return [subscriptCall.trimmedRange]
}

private func calculateRangesInside(
  labeledExpression: LabeledExprSyntax,
  previousNode: Syntax,
) -> [Range<AbsolutePosition>] {
  var ranges: [Range<AbsolutePosition>] = []

  if let label = labeledExpression.label,
    previousNode.id == label.id
  {

    ranges.append(label.trimmedRange)
  }

  let end = labeledExpression.expression.endPositionBeforeTrailingTrivia
  ranges.append(labeledExpression.positionAfterSkippingLeadingTrivia..<end)

  return ranges
}

private func calculateRangesInside(
  functionDeclaration: FunctionDeclSyntax,
  previousNode: Syntax,
) -> [Range<AbsolutePosition>] {
  var ranges: [Range<AbsolutePosition>] = []
  if previousNode.id == functionDeclaration.name.id {
    ranges.append(functionDeclaration.name.trimmedRange)
  } else if let genericClause = functionDeclaration.genericParameterClause,
    previousNode.id == genericClause.id
  {
    ranges.append(
      functionDeclaration.name.positionAfterSkippingLeadingTrivia..<genericClause.endPositionBeforeTrailingTrivia
    )
  }

  ranges.append(functionDeclaration.trimmedRange)

  return ranges
}

private func calculateRangesInside(
  genericParameterClause: GenericParameterClauseSyntax,
  previousNode: Syntax,
) -> [Range<AbsolutePosition>] {
  return [genericParameterClause.trimmedRange]
}

private func calculateRangesInside(genericParameter: GenericParameterSyntax) -> [Range<AbsolutePosition>] {
  if let parameterList = genericParameter.parent?.as(GenericParameterListSyntax.self),
    parameterList.count == 1
  {
    // include a possible trailing comma if the generic parameter is the only one in the generic clause
    return [genericParameter.trimmedRange]
  }

  let end = genericParameter.trailingComma?.position ?? genericParameter.endPositionBeforeTrailingTrivia
  return [genericParameter.positionAfterSkippingLeadingTrivia..<end]
}

private func calculateRangesInside(
  parameter: FunctionParameterSyntax,
  previousNode: Syntax,
) -> [Range<AbsolutePosition>] {
  let start = parameter.positionAfterSkippingLeadingTrivia
  let end =
    if let comma = parameter.trailingComma {
      comma.position
    } else {
      parameter.endPositionBeforeTrailingTrivia
    }
  let rangeWithoutComma = start..<end

  if previousNode.id == parameter.type.id {
    if let ellipsis = parameter.ellipsis {
      // add an additional range for selecting the ellipsis of variadic parameters
      let range = parameter.type.positionAfterSkippingLeadingTrivia..<ellipsis.endPositionBeforeTrailingTrivia
      return [range, rangeWithoutComma]
    }
    return [rangeWithoutComma]
  }

  if let defaultValue = parameter.defaultValue, previousNode.id == defaultValue.id {
    return [rangeWithoutComma]
  }

  let firstNameRange = parameter.firstName.trimmedRange

  if let secondName = parameter.secondName {
    let range = firstNameRange.lowerBound..<secondName.endPositionBeforeTrailingTrivia
    if previousNode.id == parameter.firstName.id {
      return [firstNameRange, range, rangeWithoutComma]
    } else if previousNode.id == secondName.id {
      return [secondName.trimmedRange, range, rangeWithoutComma]
    }
  }

  return [firstNameRange, rangeWithoutComma]
}

private func calculateRangesInside(
  effectSpecifiers: FunctionEffectSpecifiersSyntax,
  previousNode: Syntax,
) -> [Range<AbsolutePosition>] {
  var ranges: [Range<AbsolutePosition>] = []
  if let asyncSpecifier = effectSpecifiers.asyncSpecifier,
    previousNode.id == asyncSpecifier.id
  {
    // explicitly add a range for the async keyword token as we directly skip to the parent of the token that contained the cursor
    ranges.append(asyncSpecifier.trimmedRange)
  }

  ranges.append(effectSpecifiers.trimmedRange)

  return ranges
}

private func calculateRangesInside(closureSignature: ClosureSignatureSyntax) -> [Range<AbsolutePosition>] {
  var ranges: [Range<AbsolutePosition>] = []
  ranges.append(closureSignature.trimmedRange)

  if let closureExpression = closureSignature.parent?.as(ClosureExprSyntax.self) {
    let start = closureSignature.positionAfterSkippingLeadingTrivia
    let end = closureExpression.statements.endPositionBeforeTrailingTrivia
    ranges.append(start..<end)
  }

  return ranges
}

private func calculateRangesInside(
  enumParameter: EnumCaseParameterSyntax,
  previousNode: Syntax,
) -> [Range<AbsolutePosition>] {
  // this implementation is really similar to the one for FunctionParameterSyntax,
  // except that we don't have to deal with ellipses and have to deal with unlabeled parameters
  let start = enumParameter.positionAfterSkippingLeadingTrivia
  let end =
    if let comma = enumParameter.trailingComma {
      comma.position
    } else {
      enumParameter.endPositionBeforeTrailingTrivia
    }
  let rangeWithoutComma = start..<end

  if previousNode.id == enumParameter.type.id {
    return [rangeWithoutComma]
  }

  if let defaultValue = enumParameter.defaultValue, previousNode.id == defaultValue.id {
    return [rangeWithoutComma]
  }

  var ranges: [Range<AbsolutePosition>] = []

  if let firstName = enumParameter.firstName {
    if let secondName = enumParameter.secondName {
      let range = firstName.positionAfterSkippingLeadingTrivia..<secondName.endPositionBeforeTrailingTrivia
      if previousNode.id == firstName.id {
        ranges.append(firstName.trimmedRange)
      } else if previousNode.id == secondName.id {
        ranges.append(secondName.trimmedRange)
      }
      ranges.append(range)
    } else {
      ranges.append(firstName.trimmedRange)
    }
  }

  ranges.append(rangeWithoutComma)

  return ranges
}

private func calculateRangesInside(
  expressionList: ExprListSyntax,
  previousNode: Syntax,
) -> [Range<AbsolutePosition>] {
  guard let sequenceExpression = expressionList.parent?.as(SequenceExprSyntax.self) else {
    return [expressionList.trimmedRange]
  }

  let table = OperatorTable.standardOperators
  guard let foldedTree = try? table.foldSingle(sequenceExpression) else { return [] }

  let startInTree =
    previousNode.positionAfterSkippingLeadingTrivia - SourceLength(utf8Length: sequenceExpression.position.utf8Offset)
  let endInTree =
    previousNode.endPositionBeforeTrailingTrivia - SourceLength(utf8Length: sequenceExpression.position.utf8Offset)

  let operandNode = findCorrespondingOperandIn(
    foldedTree: Syntax(foldedTree),
    operandStart: startInTree,
    operandEnd: endInTree
  )

  var ranges: [Range<AbsolutePosition>] = []

  for node in sequence(first: operandNode, next: { $0.parent }) {
    if node.is(InfixOperatorExprSyntax.self) {
      let startPosition =
        sequenceExpression.position + SourceLength(utf8Length: node.positionAfterSkippingLeadingTrivia.utf8Offset)
      let endPosition =
        sequenceExpression.position + SourceLength(utf8Length: node.endPositionBeforeTrailingTrivia.utf8Offset)
      ranges.append(startPosition..<endPosition)
    }
  }

  if ranges.last != sequenceExpression.trimmedRange {
    ranges.append(sequenceExpression.trimmedRange)
  }

  return ranges
}

private func findCorrespondingOperandIn(
  foldedTree: Syntax,
  operandStart: AbsolutePosition,
  operandEnd: AbsolutePosition
) -> Syntax {
  var current = foldedTree
  while true {
    guard
      let child = current.children(viewMode: .sourceAccurate).first(where: {
        $0.position <= operandStart && operandEnd <= $0.endPosition
      })
    else {
      return current
    }

    if !child.is(InfixOperatorExprSyntax.self) {
      return child
    }

    current = child
  }
}

private func calculateRangesInside(patternBinding: PatternBindingSyntax) -> [Range<AbsolutePosition>] {
  guard let patternBindingList = patternBinding.parent?.as(PatternBindingListSyntax.self) else {
    return []
  }

  if patternBindingList.children(viewMode: .sourceAccurate).count > 1 {
    // special case for pattern bindings like this: `let x = 1, y = 2, z = 3`
    // here we want to be able to select only `y = 2`
    let start = patternBinding.positionAfterSkippingLeadingTrivia
    let end =
      if let comma = patternBinding.trailingComma {
        comma.position
      } else {
        patternBinding.endPositionBeforeTrailingTrivia
      }
    return [start..<end]
  }

  if let accessorBlock = patternBinding.accessorBlock {
    return [accessorBlock.trimmedRange]
  }

  // by default we don't want to create ranges for pattern bindings to avoid selecting `x = 0` in `let x = 0`
  return []
}

private func calculateRangesInside(codeBlock: CodeBlockSyntax) -> [Range<AbsolutePosition>] {
  if let ifExpression = codeBlock.parent?.as(IfExprSyntax.self),
    let elseKeyword = ifExpression.elseKeyword,
    ifExpression.elseBody?.id == codeBlock.id
  {
    // special case for if expression: when inside the else block add a range for selection `else {...}`
    return [elseKeyword.positionAfterSkippingLeadingTrivia..<codeBlock.endPositionBeforeTrailingTrivia]
  }

  return []
}

private func calculateRangesInside(forStatement: ForStmtSyntax) -> [Range<AbsolutePosition>] {
  return [
    forStatement.pattern.positionAfterSkippingLeadingTrivia..<forStatement.sequence.endPositionBeforeTrailingTrivia,
    forStatement.trimmedRange,
  ]
}

private func calculateRangesInside(
  associatedTypeDeclaration: AssociatedTypeDeclSyntax,
  previousNode: Syntax,
) -> [Range<AbsolutePosition>] {
  var ranges: [Range<AbsolutePosition>] = []
  if previousNode.id == associatedTypeDeclaration.name.id {
    ranges.append(associatedTypeDeclaration.name.trimmedRange)
  }

  ranges.append(associatedTypeDeclaration.trimmedRange)
  return ranges
}

private func calculateRangesInside(
  typeAliasDeclaration: TypeAliasDeclSyntax,
  previousNode: Syntax,
) -> [Range<AbsolutePosition>] {
  var ranges: [Range<AbsolutePosition>] = []

  if previousNode.id == typeAliasDeclaration.name.id {
    ranges.append(typeAliasDeclaration.name.trimmedRange)
  }

  ranges.append(typeAliasDeclaration.trimmedRange)

  return ranges
}

private func calculateRangesInside(dictionaryElement: DictionaryElementSyntax) -> [Range<AbsolutePosition>] {
  let start = dictionaryElement.positionAfterSkippingLeadingTrivia
  let end =
    if let trailingComma = dictionaryElement.trailingComma {
      trailingComma.positionAfterSkippingLeadingTrivia
    } else {
      dictionaryElement.positionAfterSkippingLeadingTrivia
    }
  return [start..<end]
}

private func calculateRangesInside(
  operatorDeclaration: OperatorDeclSyntax,
  previousNode: Syntax,
) -> [Range<AbsolutePosition>] {
  if previousNode.id == operatorDeclaration.name.id {
    return [operatorDeclaration.name.trimmedRange, operatorDeclaration.trimmedRange]
  }

  return [operatorDeclaration.trimmedRange]
}

private func calculateRangesInside(memberAccess: MemberAccessExprSyntax) -> [Range<AbsolutePosition>] {
  return if memberAccess.parent?.is(FunctionCallExprSyntax.self) == true {
    []
  } else {
    [memberAccess.trimmedRange]
  }
}

private func calculateRangesInside(identifierType: IdentifierTypeSyntax) -> [Range<AbsolutePosition>] {
  if identifierType.parent?.is(AttributeSyntax.self) == true {
    // for attributes we don't want to create a range for just the attribute but rather always include the `@`
    return []
  }

  return [identifierType.trimmedRange]
}

private func calculateRangesInside(availabilityArgument: AvailabilityArgumentSyntax) -> [Range<AbsolutePosition>] {
  if let trailingComma = availabilityArgument.trailingComma {
    let start = availabilityArgument.positionAfterSkippingLeadingTrivia
    let end = trailingComma.positionAfterSkippingLeadingTrivia
    return [start..<end]
  }

  return [availabilityArgument.trimmedRange]
}
