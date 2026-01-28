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

      guard let token = sourceFile.token(at: absolutePosition), let initialNode = token.parent else {
        return SelectionRange(range: position..<position)
      }

      if let selectionRange = computeSelectionRangeFor(
        position: absolutePosition,
        snapshot: snapshot,
        initialNode: initialNode
      ) {
        return selectionRange
      } else {
        return SelectionRange(range: position..<position)
      }
    }
  }
}

private func computeSelectionRangeFor(
  position: AbsolutePosition,
  snapshot: DocumentSnapshot,
  initialNode: Syntax
) -> SelectionRange? {
  var ranges: [Range<AbsolutePosition>] = []

  for node in sequence(first: initialNode, next: { $0.parent }) {
    let rangesForNode = calculateRangesFor(node: node, snapshot: snapshot, position: position)

    if rangesForNode.isEmpty {
      continue
    }

    for range in rangesForNode {
      if ranges.last == range {
        // some ast nodes have the exact same range, we just skip creating ranges for them
        continue
      }

      ranges.append(range)
    }
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
  snapshot: DocumentSnapshot,
  position: AbsolutePosition
) -> [Range<AbsolutePosition>] {
  switch node.as(SyntaxEnum.self) {

  case .stringSegment(let segment):
    return calculateRangesInside(stringSegment: segment, snapshot: snapshot, position: position)

  case .functionCallExpr(let functionCall):
    return calculateRangesInside(functionCall: functionCall, position: position)

  case .subscriptCallExpr(let subscriptCall):
    return calculateRangesInside(subscriptCall: subscriptCall, position: position)

  case .labeledExpr(let labeledExpression):
    return calculateRangesInside(labeledExpression: labeledExpression, position: position)

  case .functionDecl(let functionDeclaration):
    return calculateRangesInside(functionDeclaration: functionDeclaration, position: position)

  case .genericParameterClause(let genericParameterClause):
    return calculateRangesInside(genericParameterClause: genericParameterClause, position: position)

  case .genericParameter(let genericParameter):
    return calculateRangesInside(genericParameter: genericParameter)

  case .functionParameter(let parameter):
    return calculateRangesInside(parameter: parameter, position: position)

  case .functionSignature(let signature):
    return calculateRangesInside(signature: signature, position: position)

  case .closureSignature(let closureSignature):
    return calculateRangesInside(closureSignature: closureSignature)

  case .classDecl(let classDeclaration):
    return calculateRangesInside(classDeclaration: classDeclaration, position: position)

  case .structDecl(let structDeclaration):
    return calculateRangesInside(structDeclaration: structDeclaration, position: position)

  case .protocolDecl(let protocolDeclaration):
    return calculateRangesInside(protocolDeclaration: protocolDeclaration, position: position)

  case .extensionDecl(let extensionDeclaration):
    return calculateRangesInside(extensionDeclaration: extensionDeclaration)

  case .enumCaseParameter(let enumParameter):
    return calculateRangesInside(enumParameter: enumParameter, position: position)

  case .sequenceExpr(let sequenceExpression):
    return calculateRangesInside(sequenceExpression: sequenceExpression, position: position)

  case .patternBinding(let patternBinding):
    return calculateRangesInside(patternBinding: patternBinding)

  case .codeBlock(let codeBlock):
    return calculateRangesInside(codeBlock: codeBlock)

  case .forStmt(let forStatement):
    return calculateRangesInside(forStatement: forStatement)

  case .associatedTypeDecl(let associatedTypeDeclaration):
    return calculateRangesInside(associatedTypeDeclaration: associatedTypeDeclaration, position: position)

  case .typeAliasDecl(let typeAliasDeclaration):
    return calculateRangesInside(typeAliasDeclaration: typeAliasDeclaration, position: position)

  case .dictionaryElement(let dictionaryElement):
    return calculateRangesInside(dictionaryElement: dictionaryElement)

  case .operatorDecl(let operatorDeclaration):
    return calculateRangesInside(operatorDeclaration: operatorDeclaration, position: position)

  case .memberAccessExpr(let memberAccess):
    return calculateRangesInside(memberAccess: memberAccess)

  case .identifierType(let identifierType):
    return calculateRangesInside(identifierType: identifierType)

  case .availabilityArgument(let availabilityArgument):
    return calculateRangesInside(availabilityArgument: availabilityArgument)

  case .patternBindingList, .initializerClause, .matchingPatternCondition, .exprList,
    .accessorDeclList, .functionParameterClause, .functionEffectSpecifiers, .switchCaseLabel, .switchCaseList,
    .inheritanceClause, .inheritedType, .memberBlockItemList, .memberBlock, .enumCaseParameterClause,
    .optionalChainingExpr, .tuplePatternElement, .arrayElement, .keyPathComponent, .keyPathComponentList:
    return []

  default:
    return [node.trimmedRange]
  }
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
  position: AbsolutePosition
) -> [Range<AbsolutePosition>] {
  if let memberAccess = functionCall.calledExpression.as(MemberAccessExprSyntax.self),
    functionCall.parent?.as(ExpressionPatternSyntax.self) == nil,
    functionCall.arguments.trimmedRange.contains(position)
      || functionCall.trailingClosure?.trimmedRange.contains(position) ?? false
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

  if let rightParen = functionCall.rightParen,
    rightParen.trimmedRange.contains(position),
    let lastArgument = functionCall.arguments.last
  {
    // special case for when the cursor is right before the closing paren, like this: `test(a: 1|)`
    // we still want to select the parameters first
    var ranges: [Range<AbsolutePosition>] = []

    ranges.append(lastArgument.expression.trimmedRange)
    ranges.append(contentsOf: calculateRangesInside(labeledExpression: lastArgument, position: position))
    ranges.append(functionCall.arguments.trimmedRange)
    ranges.append(functionCall.trimmedRange)

    return ranges
  }

  // the default case: just create a range for the function call node
  return [functionCall.trimmedRange]
}

private func calculateRangesInside(
  subscriptCall: SubscriptCallExprSyntax,
  position: AbsolutePosition
) -> [Range<AbsolutePosition>] {
  if subscriptCall.arguments.trimmedRange.contains(position) {
    let start = subscriptCall.leftSquare.positionAfterSkippingLeadingTrivia
    let end = subscriptCall.rightSquare.endPositionBeforeTrailingTrivia
    return [start..<end, subscriptCall.trimmedRange]
  }

  return [subscriptCall.trimmedRange]
}

private func calculateRangesInside(
  labeledExpression: LabeledExprSyntax,
  position: AbsolutePosition
) -> [Range<AbsolutePosition>] {
  var ranges: [Range<AbsolutePosition>] = []

  if let label = labeledExpression.label,
    label.trimmedRange.contains(position)
  {

    ranges.append(label.trimmedRange)
  }

  let end = labeledExpression.expression.endPositionBeforeTrailingTrivia
  ranges.append(labeledExpression.positionAfterSkippingLeadingTrivia..<end)

  return ranges
}

private func calculateRangesInside(
  functionDeclaration: FunctionDeclSyntax,
  position: AbsolutePosition
) -> [Range<AbsolutePosition>] {
  var ranges: [Range<AbsolutePosition>] = []
  if functionDeclaration.name.trimmedRange.contains(position) {
    ranges.append(functionDeclaration.name.trimmedRange)
  } else if let genericClause = functionDeclaration.genericParameterClause,
    genericClause.trimmedRange.contains(position)
  {
    ranges.append(
      functionDeclaration.name.positionAfterSkippingLeadingTrivia..<genericClause.endPositionBeforeTrailingTrivia
    )
  } else if functionDeclaration.signature.parameterClause.rightParen.trimmedRange.contains(position),
    let lastArgument = functionDeclaration.signature.parameterClause.parameters.last
  {
    // special case for when the cursor is directly before the closing paren, like this: `a: Int|)`
    ranges.append(lastArgument.type.trimmedRange)
    // using this position is a bit of a hack, but it is needed as the calculateRangesInside() function
    // uses the position information to check which ranges it needs to create
    // if we provided the correct position the function would return the wrong ranges
    let shiftedPosition = lastArgument.endPositionBeforeTrailingTrivia.advanced(by: -1)
    ranges.append(contentsOf: calculateRangesInside(parameter: lastArgument, position: shiftedPosition))

    ranges.append(functionDeclaration.signature.parameterClause.parameters.trimmedRange)
  }

  ranges.append(functionDeclaration.trimmedRange)

  return ranges
}

private func calculateRangesInside(
  genericParameterClause: GenericParameterClauseSyntax,
  position: AbsolutePosition
) -> [Range<AbsolutePosition>] {
  if genericParameterClause.rightAngle.trimmedRange.contains(position) {
    // special case for when the cursor is directly before the angle bracket, like this: `<T|>`
    // in this case we still want to have a selection range for selecting `T`
    var ranges: [Range<AbsolutePosition>] = []
    if let lastParameter = genericParameterClause.parameters.last {
      if lastParameter.trailingComma == nil {
        ranges.append(contentsOf: calculateRangesInside(genericParameter: lastParameter))
      } else {
        // if the parameter has a trailing comma like this: `<T,|>` we don't create an additional range
        ranges.append(lastParameter.trimmedRange)
      }
    }
    ranges.append(genericParameterClause.trimmedRange)
    return ranges
  }

  return [genericParameterClause.trimmedRange]
}

private func calculateRangesInside(genericParameter: GenericParameterSyntax) -> [Range<AbsolutePosition>] {
  let end = genericParameter.trailingComma?.position ?? genericParameter.endPositionBeforeTrailingTrivia
  return [genericParameter.positionAfterSkippingLeadingTrivia..<end]
}

private func calculateRangesInside(
  parameter: FunctionParameterSyntax,
  position: AbsolutePosition
) -> [Range<AbsolutePosition>] {
  let start = parameter.positionAfterSkippingLeadingTrivia
  let end =
    if let comma = parameter.trailingComma {
      comma.position
    } else {
      parameter.endPositionBeforeTrailingTrivia
    }
  let rangeWithoutComma = start..<end

  if parameter.type.trimmedRange.contains(position) {
    if let ellipsis = parameter.ellipsis {
      // add an additional range for selecting the ellipsis of variadic parameters
      let range = parameter.type.positionAfterSkippingLeadingTrivia..<ellipsis.endPositionBeforeTrailingTrivia
      return [range, rangeWithoutComma]
    }
    return [rangeWithoutComma]
  }

  if let defaultValue = parameter.defaultValue, defaultValue.trimmedRange.contains(position) {
    return [rangeWithoutComma]
  }

  let firstNameRange = parameter.firstName.trimmedRange

  if let secondName = parameter.secondName {
    let range = firstNameRange.lowerBound..<secondName.endPositionBeforeTrailingTrivia
    if parameter.firstName.trimmedRange.contains(position) {
      return [firstNameRange, range, rangeWithoutComma]
    } else if secondName.trimmedRange.contains(position) {
      return [secondName.trimmedRange, range, rangeWithoutComma]
    }
  }

  return [firstNameRange, rangeWithoutComma]
}

private func calculateRangesInside(
  signature: FunctionSignatureSyntax,
  position: AbsolutePosition
) -> [Range<AbsolutePosition>] {
  var ranges: [Range<AbsolutePosition>] = []
  if let effectSpecifiers = signature.effectSpecifiers,
    let asyncSpecifier = effectSpecifiers.asyncSpecifier,
    asyncSpecifier.trimmedRange.contains(position)
  {
    // explicitly add a range for the async keyword token as we directly skip to the parent of the token that contained the cursor
    ranges.append(asyncSpecifier.trimmedRange)
  }

  if let effectSpecifiers = signature.effectSpecifiers,
    let returnClause = signature.returnClause
  {
    if effectSpecifiers.trimmedRange.contains(position) {
      ranges.append(effectSpecifiers.trimmedRange)
      ranges.append(effectSpecifiers.positionAfterSkippingLeadingTrivia..<returnClause.endPositionBeforeTrailingTrivia)
    } else if returnClause.trimmedRange.contains(position) {
      ranges.append(effectSpecifiers.positionAfterSkippingLeadingTrivia..<returnClause.endPositionBeforeTrailingTrivia)
    }
  }

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
  classDeclaration: ClassDeclSyntax,
  position: AbsolutePosition
) -> [Range<AbsolutePosition>] {
  var ranges: [Range<AbsolutePosition>] = []

  if classDeclaration.name.trimmedRange.contains(position) {
    ranges.append(classDeclaration.name.trimmedRange)
  }

  if let inheritanceClause = classDeclaration.inheritanceClause {
    let start = classDeclaration.name.positionAfterSkippingLeadingTrivia
    let end = inheritanceClause.endPositionBeforeTrailingTrivia
    ranges.append(start..<end)
  }

  ranges.append(classDeclaration.trimmedRange)

  return ranges
}

private func calculateRangesInside(
  structDeclaration: StructDeclSyntax,
  position: AbsolutePosition
) -> [Range<AbsolutePosition>] {
  var ranges: [Range<AbsolutePosition>] = []

  if structDeclaration.name.trimmedRange.contains(position) {
    ranges.append(structDeclaration.name.trimmedRange)
  } else if let genericClause = structDeclaration.genericParameterClause,
    genericClause.trimmedRange.contains(position)
  {
    let start = structDeclaration.name.positionAfterSkippingLeadingTrivia
    let end = genericClause.endPositionBeforeTrailingTrivia
    ranges.append(start..<end)
  }

  if let inheritanceClause = structDeclaration.inheritanceClause {
    let start = structDeclaration.name.positionAfterSkippingLeadingTrivia
    let end = inheritanceClause.endPositionBeforeTrailingTrivia
    ranges.append(start..<end)
  }

  ranges.append(structDeclaration.trimmedRange)

  return ranges
}

private func calculateRangesInside(
  protocolDeclaration: ProtocolDeclSyntax,
  position: AbsolutePosition
) -> [Range<AbsolutePosition>] {
  var ranges: [Range<AbsolutePosition>] = []

  if protocolDeclaration.name.trimmedRange.contains(position) {
    ranges.append(protocolDeclaration.name.trimmedRange)
  }

  if let inheritanceClause = protocolDeclaration.inheritanceClause {
    let start = protocolDeclaration.name.positionAfterSkippingLeadingTrivia
    let end = inheritanceClause.endPositionBeforeTrailingTrivia
    ranges.append(start..<end)
  }

  ranges.append(protocolDeclaration.trimmedRange)

  return ranges
}

private func calculateRangesInside(extensionDeclaration: ExtensionDeclSyntax) -> [Range<AbsolutePosition>] {
  var ranges: [Range<AbsolutePosition>] = []

  if let inheritanceClause = extensionDeclaration.inheritanceClause {
    let start = extensionDeclaration.extendedType.positionAfterSkippingLeadingTrivia
    let end = inheritanceClause.endPositionBeforeTrailingTrivia
    ranges.append(start..<end)

    if let whereClause = extensionDeclaration.genericWhereClause {
      ranges.append(start..<whereClause.endPositionBeforeTrailingTrivia)
    }
  } else if let whereClause = extensionDeclaration.genericWhereClause {
    let start = extensionDeclaration.extendedType.positionAfterSkippingLeadingTrivia
    let end = whereClause.endPositionBeforeTrailingTrivia
    ranges.append(start..<end)
  }

  ranges.append(extensionDeclaration.trimmedRange)

  return ranges
}

private func calculateRangesInside(
  enumParameter: EnumCaseParameterSyntax,
  position: AbsolutePosition
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

  if enumParameter.type.trimmedRange.contains(position) {
    return [rangeWithoutComma]
  }

  if let defaultValue = enumParameter.defaultValue, defaultValue.trimmedRange.contains(position) {
    return [rangeWithoutComma]
  }

  var ranges: [Range<AbsolutePosition>] = []

  if let firstName = enumParameter.firstName {
    if let secondName = enumParameter.secondName {
      let range = firstName.positionAfterSkippingLeadingTrivia..<secondName.endPositionBeforeTrailingTrivia
      if firstName.trimmedRange.contains(position) {
        ranges.append(firstName.trimmedRange)
      } else if secondName.trimmedRange.contains(position) {
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
  sequenceExpression: SequenceExprSyntax,
  position: AbsolutePosition
) -> [Range<AbsolutePosition>] {
  let table = OperatorTable.standardOperators
  guard let foldedTree = try? table.foldSingle(sequenceExpression) else { return [] }

  let positionInFoldedTree = position - SourceLength(utf8Length: sequenceExpression.position.utf8Offset)

  let operandNode = findCorrespondingOperandIn(
    foldedTree: Syntax(foldedTree),
    positionInFoldedTree: positionInFoldedTree
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

private func findCorrespondingOperandIn(foldedTree: Syntax, positionInFoldedTree: AbsolutePosition) -> Syntax {
  var current = foldedTree
  while true {
    guard
      let child = current.children(viewMode: .sourceAccurate).first(where: {
        $0.position <= positionInFoldedTree && positionInFoldedTree < $0.endPosition
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
  position: AbsolutePosition
) -> [Range<AbsolutePosition>] {
  var ranges: [Range<AbsolutePosition>] = []
  if associatedTypeDeclaration.name.trimmedRange.contains(position) {
    ranges.append(associatedTypeDeclaration.name.trimmedRange)
  }

  ranges.append(associatedTypeDeclaration.trimmedRange)
  return ranges
}

private func calculateRangesInside(
  typeAliasDeclaration: TypeAliasDeclSyntax,
  position: AbsolutePosition
) -> [Range<AbsolutePosition>] {
  var ranges: [Range<AbsolutePosition>] = []

  if typeAliasDeclaration.name.trimmedRange.contains(position) {
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
  position: AbsolutePosition
) -> [Range<AbsolutePosition>] {
  if operatorDeclaration.name.trimmedRange.contains(position) {
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
