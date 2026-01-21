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
      let offset = snapshot.absolutePosition(of: position)

      guard let token = sourceFile.token(at: offset), let initialNode = token.parent else {
        return SelectionRange(range: position..<position)
      }

      return computeSelectionRangeFor(position: position, snapshot: snapshot, initialNode: initialNode)
    }
  }
}

private func computeSelectionRangeFor(
  position: Position,
  snapshot: DocumentSnapshot,
  initialNode: Syntax
) -> SelectionRange {
  var ranges: [Range<Position>] = []

  for node in sequence(first: initialNode, next: { $0.parent }) {
    if shouldSkipNode(node) {
      continue
    }

    let start = snapshot.position(of: node.positionAfterSkippingLeadingTrivia)
    let end = snapshot.position(of: node.endPositionBeforeTrailingTrivia)
    let range = start..<end

    if ranges.last == range {
      // some ast nodes have the exact same range, we just skip creating ranges for them
      continue
    }

    if let stringSegment = node.as(StringSegmentSyntax.self) {
      ranges.append(
        contentsOf: calculateRangesInside(stringSegment: stringSegment, snapshot: snapshot, position: position)
      )
    }

    ranges.append(range)
  }

  var selectionRange: SelectionRange? = nil
  for range in ranges.reversed() {
    selectionRange = SelectionRange(range: range, parent: selectionRange)
  }

  return selectionRange!
}

private func shouldSkipNode(_ node: Syntax) -> Bool {
  return switch node.as(SyntaxEnum.self) {
  case .patternBinding, .patternBindingList, .initializerClause:
    true

  default:
    false
  }
}

// we return a list here as this allows us to possibly return multiple ranges in the future
private func calculateRangesInside(
  stringSegment: StringSegmentSyntax,
  snapshot: DocumentSnapshot,
  position: Position
) -> [Range<Position>] {
  let absolutePosition = snapshot.absolutePosition(of: position)
  let offsetInString = absolutePosition.utf8Offset - stringSegment.positionAfterSkippingLeadingTrivia.utf8Offset

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

  let startPosition = snapshot.position(
    of: stringSegment.positionAfterSkippingLeadingTrivia.advanced(by: startOffsetInString)
  )
  let endPosition = snapshot.position(
    of: stringSegment.positionAfterSkippingLeadingTrivia.advanced(by: endOffsetInString)
  )

  return [startPosition..<endPosition]
}
