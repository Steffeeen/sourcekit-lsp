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

  case .patternBinding, .patternBindingList, .initializerClause, .memberAccessExpr, .matchingPatternCondition:
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
