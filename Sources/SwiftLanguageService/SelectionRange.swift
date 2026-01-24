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

  case .labeledExpr(let labeledExpression):
    return [
      labeledExpression
        .positionAfterSkippingLeadingTrivia..<labeledExpression.expression.endPositionBeforeTrailingTrivia
    ]

  case .stringSegment(let segment):
    return calculateRangesInside(stringSegment: segment, snapshot: snapshot, position: position)

  case .functionCallExpr(let functionCall):
    return calculateRangesInside(functionCall: functionCall, position: position)

  case .functionParameter(let parameter):
    return calculateRangesInside(parameter: parameter, position: position)

  case .sequenceExpr(let sequenceExpression):
    return calculateRangesInside(sequenceExpression: sequenceExpression, position: position)

  case .patternBinding(let patternBinding):
    return calculateRangesInside(patternBinding: patternBinding)

  case .codeBlock(let codeBlock):
    return calculateRangesInside(codeBlock: codeBlock)

  case .patternBindingList, .initializerClause, .memberAccessExpr, .matchingPatternCondition,
    .exprList, .accessorDeclList, .functionParameterClause, .functionSignature:
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

  // the default case: just create a range for the function call node
  return [functionCall.trimmedRange]
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
