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

      guard let token = findIntuitiveToken(in: sourceFile, at: absolutePosition) else {
        return SelectionRange(range: position..<position)
      }

      guard
        let selectionRange = computeSelectionRangeFor(
          position: absolutePosition,
          snapshot: snapshot,
          token: token
        )
      else {
        return SelectionRange(range: position..<position)
      }

      return selectionRange
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
    // The cursor is at the start of a boundary token (e.g. `test(a: 3, b: 2|)`)
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
  token: TokenSyntax
) -> SelectionRange? {
  var ranges: [Range<AbsolutePosition>] = []

  // The first node we call calculateRangesFor() for is never a token. This forces us to sometimes explicitly create
  // selection ranges for tokens, for example in calculateRangesForTypeOrFunctionDeclaration().
  //
  // The alternative would be to use the token as the child and parent simultaneously in the first iteration.
  // This would however lead to extra special-casing in other places, for example for attributes. For attributes we
  // immediately want to select the entire attribute including the `@`. We thus don't want to create a selection range
  // for the identifier token. This ensure this we would need to special-case the range creation of the identifier
  // token to check what kind of parent it has.
  var child = Syntax(token)
  while let parent = child.parent {
    let rangesForNode = calculateRangesFor(node: parent, previousNode: child, snapshot: snapshot, position: position)

    for range in rangesForNode {
      if ranges.last == range {
        // Some AST nodes have the exact same range, we just skip creating ranges for them
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
  case .stringSegment(let stringSegmentSyntax):
    // We have to use custom logic for string segments as they need the position.
    // We cannot provide the position in the protocol as it may not always be correct due to the logic in findIntuitiveToken().
    // For the string segment this does not matter as when we encounter a string segment node we can be sure that
    // findIntuitiveToken() always returned the original token.
    return calculateSelectionRangesForStringSegment(stringSegment: stringSegmentSyntax, position: position)

  case .patternBindingList, .initializerClause, .matchingPatternCondition, .sequenceExpr,
    .accessorDeclList, .functionParameterClause, .functionSignature, .switchCaseLabel, .switchCaseList,
    .inheritedType, .memberBlockItemList, .memberBlock, .enumCaseParameterClause,
    .optionalChainingExpr, .tuplePatternElement, .arrayElement, .keyPathComponent, .keyPathComponentList:
    return []

  default:
    if node.isProtocol((any DeclGroupSyntax).self)
      || node.is(TypeAliasDeclSyntax.self)
      || node.is(FunctionDeclSyntax.self)
    {
      let name = Syntax(node.asProtocol((any NamedDeclSyntax).self)?.name)
      let type = Syntax(node.as(ExtensionDeclSyntax.self)?.extendedType)
      let genericParameterClause = node.asProtocol((any WithGenericParametersSyntax).self)?.genericParameterClause

      if let nameOrType = name ?? type {
        return calculateRangesForTypeOrFunctionDeclaration(
          declaration: node,
          previousNode: previousNode,
          nameOrType: nameOrType,
          genericParameters: genericParameterClause
        )
      }
    }

    if let provider = node.asProtocol((any SyntaxProtocol).self) as? (any SelectionRangeProvider) {
      return provider.calculateSelectionRanges(previousNode: previousNode)
    }

    return [node.trimmedRange]
  }
}

private func calculateSelectionRangesForStringSegment(
  stringSegment: StringSegmentSyntax,
  position: AbsolutePosition
) -> [Range<AbsolutePosition>] {
  // For string segments we first want to select just the word under the cursor.
  // To determine words we use a simple heuristic: expand the selection until we hit any whitespace character.

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

private func calculateRangesForTypeOrFunctionDeclaration(
  declaration: some SyntaxProtocol,
  previousNode: some SyntaxProtocol,
  nameOrType: some SyntaxProtocol,
  genericParameters: GenericParameterClauseSyntax?,
) -> [Range<AbsolutePosition>] {
  // If we started the selection in either the name (or type in the case of extensions)
  // or the generic parameter clause of a declaration we want to have a selection range
  // for just the name (type) and generic parameter clause.
  //
  // Example: given `struct Test<|T> {}` we want to have a selection range for `Test<T>`
  //
  // As the name (type) and generic parameter clause are siblings in the declaration nodes
  // we have to special-case them.

  var ranges: [Range<AbsolutePosition>] = []

  if previousNode.id == nameOrType.id {
    // As the name is a token we have to explicitly add a range here for selecting just the name
    ranges.append(nameOrType.trimmedRange)

    if let genericParameters = genericParameters {
      let start = nameOrType.positionAfterSkippingLeadingTrivia
      let end = genericParameters.endPositionBeforeTrailingTrivia
      ranges.append(start..<end)
    }
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

private protocol SelectionRangeProvider: SyntaxProtocol {
  func calculateSelectionRanges(previousNode: Syntax) -> [Range<AbsolutePosition>]
}

extension FunctionCallExprSyntax: SelectionRangeProvider {
  func calculateSelectionRanges(previousNode: Syntax) -> [Range<AbsolutePosition>] {
    if let memberAccess = self.calledExpression.as(MemberAccessExprSyntax.self),
      !(self.parent?.is(ExpressionPatternSyntax.self) ?? false),
      previousNode.id == self.arguments.id
        || previousNode.id == self.trailingClosure?.id
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
        memberAccess.declName.positionAfterSkippingLeadingTrivia..<self.endPositionBeforeTrailingTrivia,
        self.trimmedRange,
      ]
    }

    // the default case: just create a range for the function call node
    return [self.trimmedRange]
  }
}

extension SubscriptCallExprSyntax: SelectionRangeProvider {
  func calculateSelectionRanges(previousNode: Syntax) -> [Range<AbsolutePosition>] {
    // For subscript calls we want to have a selection range for the entire subscript operator
    // including the `[]`
    // example: given `matrix[2, |3]` we want to be able to select `[2, 3]`

    if previousNode.id == self.arguments.id {
      let start = self.leftSquare.positionAfterSkippingLeadingTrivia
      let end = self.rightSquare.endPositionBeforeTrailingTrivia
      return [start..<end, self.trimmedRange]
    }

    return [self.trimmedRange]
  }
}

extension LabeledExprSyntax: SelectionRangeProvider {
  func calculateSelectionRanges(previousNode: Syntax) -> [Range<AbsolutePosition>] {
    // for labeled expressions we want to be able to select just the label and expression without the comma
    var ranges: [Range<AbsolutePosition>] = []

    if let label = self.label, previousNode.id == label.id {
      // the label is a token, so we have to manually add a selection range for it
      ranges.append(label.trimmedRange)
    }

    let end = self.expression.endPositionBeforeTrailingTrivia
    ranges.append(self.positionAfterSkippingLeadingTrivia..<end)

    return ranges
  }
}

extension GenericParameterSyntax: SelectionRangeProvider {
  func calculateSelectionRanges(previousNode: Syntax) -> [Range<AbsolutePosition>] {
    // don't include the trailing comma in the selection, except if the parameter is the only one
    if let parameterList = self.parent?.as(GenericParameterListSyntax.self),
      parameterList.count == 1
    {
      return [self.trimmedRange]
    }

    let end = self.trailingComma?.position ?? self.endPositionBeforeTrailingTrivia
    return [self.positionAfterSkippingLeadingTrivia..<end]
  }
}

extension FunctionParameterSyntax: SelectionRangeProvider {
  func calculateSelectionRanges(previousNode: Syntax) -> [Range<AbsolutePosition>] {
    // Function parameters have two special cases:
    // - If the cursor is in the type and the type includes an ellipsis we want to have a range for the type and ellipsis
    // - If the parameter has two names we want to have a range for selecting both names
    let start = self.positionAfterSkippingLeadingTrivia
    let end = self.trailingComma?.position ?? self.endPositionBeforeTrailingTrivia
    let rangeWithoutComma = start..<end

    if previousNode.id == self.type.id {
      if let ellipsis = self.ellipsis {
        // Add an additional range for selecting the ellipsis of variadic parameters.
        let range = self.type.positionAfterSkippingLeadingTrivia..<ellipsis.endPositionBeforeTrailingTrivia
        return [range, rangeWithoutComma]
      }
      return [rangeWithoutComma]
    }

    if let defaultValue = self.defaultValue, previousNode.id == defaultValue.id {
      return [rangeWithoutComma]
    }

    let firstNameRange = self.firstName.trimmedRange

    if let secondName = self.secondName {
      // If the parameter has two names we return a range for just the name under the cursor,
      // both names and the entire parameter.
      let range = firstNameRange.lowerBound..<secondName.endPositionBeforeTrailingTrivia
      if previousNode.id == self.firstName.id {
        return [firstNameRange, range, rangeWithoutComma]
      } else if previousNode.id == secondName.id {
        return [secondName.trimmedRange, range, rangeWithoutComma]
      }
    }

    return [firstNameRange, rangeWithoutComma]
  }
}

extension FunctionEffectSpecifiersSyntax: SelectionRangeProvider {
  func calculateSelectionRanges(previousNode: Syntax) -> [Range<AbsolutePosition>] {
    var ranges: [Range<AbsolutePosition>] = []
    if let asyncSpecifier = self.asyncSpecifier,
      previousNode.id == asyncSpecifier.id
    {
      // explicitly add a range for the async keyword token as the first AST node we look at is never a token.
      ranges.append(asyncSpecifier.trimmedRange)
    }

    ranges.append(self.trimmedRange)

    return ranges
  }
}

extension ClosureSignatureSyntax: SelectionRangeProvider {
  func calculateSelectionRanges(previousNode: Syntax) -> [Range<AbsolutePosition>] {
    // For closure signatures we add a selection for the signature + body content
    var ranges: [Range<AbsolutePosition>] = []
    ranges.append(self.trimmedRange)

    if let closureExpression = self.parent?.as(ClosureExprSyntax.self) {
      let start = self.positionAfterSkippingLeadingTrivia
      let end = closureExpression.statements.endPositionBeforeTrailingTrivia
      ranges.append(start..<end)
    }

    return ranges
  }
}

extension EnumCaseParameterSyntax: SelectionRangeProvider {
  func calculateSelectionRanges(previousNode: Syntax) -> [Range<AbsolutePosition>] {
    // For enum case parameters we also add a range for selecting the names of the parameter, similar to function parameters

    // This implementation is really similar to the one for FunctionParameterSyntax,
    // except that we don't have to deal with ellipses and have to deal with unlabeled parameters
    let start = self.positionAfterSkippingLeadingTrivia
    let end = self.trailingComma?.position ?? self.endPositionBeforeTrailingTrivia
    let rangeWithoutComma = start..<end

    if previousNode.id == self.type.id {
      return [rangeWithoutComma]
    }

    if let defaultValue = self.defaultValue, previousNode.id == defaultValue.id {
      return [rangeWithoutComma]
    }

    var ranges: [Range<AbsolutePosition>] = []

    if let firstName = self.firstName {
      if let secondName = self.secondName {
        // The parameter has two names, first select the one with the cursor in it and then both
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
}

extension ExprListSyntax: SelectionRangeProvider {
  func calculateSelectionRanges(previousNode: Syntax) -> [Range<AbsolutePosition>] {
    // Expression lists are more complex to deal with as we first have to convert the expression list into the
    // corresponding tree. We can then find the node the cursor is on in the tree and walk the tree up to its root
    guard let sequenceExpression = self.parent?.as(SequenceExprSyntax.self) else {
      return [self.trimmedRange]
    }

    let table = OperatorTable.standardOperators
    guard let foldedTree = try? table.foldSingle(sequenceExpression) else { return [] }

    let foldedTreeOffset = SourceLength(utf8Length: sequenceExpression.position.utf8Offset)

    let startInTree = previousNode.positionAfterSkippingLeadingTrivia - foldedTreeOffset
    let endInTree = previousNode.endPositionBeforeTrailingTrivia - foldedTreeOffset

    let operandNode = findCorrespondingOperandIn(
      foldedTree: Syntax(foldedTree),
      operandStart: startInTree,
      operandEnd: endInTree
    )

    var ranges: [Range<AbsolutePosition>] = []

    for node in sequence(first: operandNode, next: \.parent) {
      if node.is(InfixOperatorExprSyntax.self) {
        let startPosition = node.positionAfterSkippingLeadingTrivia + foldedTreeOffset
        let endPosition = node.endPositionBeforeTrailingTrivia + foldedTreeOffset
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
      // walk the tree downwards, following the nodes which contain the operand node
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
}

extension PatternBindingSyntax: SelectionRangeProvider {
  func calculateSelectionRanges(previousNode: Syntax) -> [Range<AbsolutePosition>] {
    // For pattern bindings we special-case depending on whether we have a single pattern binding or multiple
    guard let patternBindingList = self.parent?.as(PatternBindingListSyntax.self) else {
      return []
    }

    if patternBindingList.children(viewMode: .sourceAccurate).count > 1 {
      // special case for pattern bindings like this: `let x = 1, y = 2, z = 3`
      // here we want to be able to select only `y = 2`
      let start = self.positionAfterSkippingLeadingTrivia
      let end = self.trailingComma?.position ?? self.endPositionBeforeTrailingTrivia
      return [start..<end]
    }

    // by default we don't want to create ranges for pattern bindings to avoid selecting `x = 0` in `let x = 0`
    return []
  }
}

extension CodeBlockSyntax: SelectionRangeProvider {
  func calculateSelectionRanges(previousNode: Syntax) -> [Range<AbsolutePosition>] {
    if let ifExpression = self.parent?.as(IfExprSyntax.self),
      let elseKeyword = ifExpression.elseKeyword,
      ifExpression.elseBody?.id == self.id
    {
      // special case for if expression: when inside the else block add a range for selecting `else {...}`
      return [elseKeyword.positionAfterSkippingLeadingTrivia..<self.endPositionBeforeTrailingTrivia]
    }

    return []
  }
}

extension ForStmtSyntax: SelectionRangeProvider {
  func calculateSelectionRanges(previousNode: Syntax) -> [Range<AbsolutePosition>] {
    // For statements get an extra range for selecting from the pattern until the sequence,
    // i.e. selecting `i in 1...3` in `for i in 1...3 {}`
    // As the for statement can have a lot of immediate children, more special cases can be added here in the future,
    // for example for the keywords before the pattern
    var ranges: [Range<AbsolutePosition>] = []

    if previousNode.id == self.pattern.id || previousNode.id == self.typeAnnotation?.id
      || previousNode.id == self.inKeyword.id || previousNode.id == self.sequence.id
    {
      let range = self.pattern.positionAfterSkippingLeadingTrivia..<self.sequence.endPositionBeforeTrailingTrivia
      ranges.append(range)
    }

    ranges.append(self.trimmedRange)

    return ranges
  }
}

extension AssociatedTypeDeclSyntax: SelectionRangeProvider {
  func calculateSelectionRanges(previousNode: Syntax) -> [Range<AbsolutePosition>] {
    var ranges: [Range<AbsolutePosition>] = []
    if previousNode.id == self.name.id {
      // As name is a token, we have to explicitly add a range for it
      ranges.append(self.name.trimmedRange)
    }

    ranges.append(self.trimmedRange)
    return ranges
  }
}

extension DictionaryElementSyntax: SelectionRangeProvider {
  func calculateSelectionRanges(previousNode: Syntax) -> [Range<AbsolutePosition>] {
    let start = self.positionAfterSkippingLeadingTrivia
    let end = self.trailingComma?.position ?? self.endPositionBeforeTrailingTrivia
    return [start..<end]
  }
}

extension OperatorDeclSyntax: SelectionRangeProvider {
  func calculateSelectionRanges(previousNode: Syntax) -> [Range<AbsolutePosition>] {
    if previousNode.id == self.name.id {
      // As name is a token, we have to explicitly add a range for it
      return [self.name.trimmedRange, self.trimmedRange]
    }

    return [self.trimmedRange]
  }
}

extension MemberAccessExprSyntax: SelectionRangeProvider {
  func calculateSelectionRanges(previousNode: Syntax) -> [Range<AbsolutePosition>] {
    if self.parent?.is(FunctionCallExprSyntax.self) ?? false {
      // If the member access is part of a function call, we don't return any range as this case is handled in the
      // FunctionCallExprSyntax extension
      return []
    }

    return [self.trimmedRange]
  }
}

extension IdentifierTypeSyntax: SelectionRangeProvider {
  func calculateSelectionRanges(previousNode: Syntax) -> [Range<AbsolutePosition>] {
    if self.parent?.is(AttributeSyntax.self) ?? false {
      // for attributes we don't want to create a range for just the attribute but rather always include the `@`
      return []
    }

    return [self.trimmedRange]
  }
}

extension AvailabilityArgumentSyntax: SelectionRangeProvider {
  func calculateSelectionRanges(previousNode: Syntax) -> [Range<AbsolutePosition>] {
    if let trailingComma = self.trailingComma {
      let start = self.positionAfterSkippingLeadingTrivia
      let end = trailingComma.positionAfterSkippingLeadingTrivia
      return [start..<end]
    }

    return [self.trimmedRange]
  }
}
