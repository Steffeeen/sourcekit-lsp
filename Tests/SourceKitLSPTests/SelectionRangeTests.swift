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

import Foundation
@_spi(SourceKitLSP) import LanguageServerProtocol
import SKTestSupport
import SKUtilities
import Testing

@Test
func testFunctionCallParameterExplicit() async throws {
  try await testSelectionRange(
    markedSource: """
      func a() {
        b(c, d: 1️⃣320)
      }
      """,
    expectedSelections: [
      "320",
      "d: 320",
      "c, d: 320",
      "b(c, d: 320)",
    ]
  )
}

@Test
func testStringLiteralWithCursorInWord() async throws {
  try await testSelectionRange(
    markedSource: """
      let a = "Hel1️⃣lo, World!"
      """,
    expectedSelections: [
      "Hello,",
      "Hello, World!",
      "\"Hello, World!\"",
    ]
  )
}

@Test
func testStringLiteralWithCursorInWhitespace() async throws {
  try await testSelectionRange(
    markedSource: """
      let a = "Hello,1️⃣ World!"
      """,
    expectedSelections: [
      "Hello, World!",
      "\"Hello, World!\"",
      "let a = \"Hello, World!\"",
    ]
  )
}

@Test
func testStringLiteralWithStringInterpolation() async throws {
  try await testSelectionRange(
    markedSource: """
      func a() {
        let a = "Hello \\(w1️⃣o)rld"
      }
      """,
    expectedSelections: [
      "wo",
      "\\(wo)",
      "Hello \\(wo)rld",
    ]
  )
}

@Test
func testMultipleCursors() async throws {
  try await testSelectionRange(
    markedSource: """
        let a = "Hel1️⃣lo, World!"
        let b = "Hel2️⃣lo, World!"
        let c = "Hel3️⃣lo, World!"
      """,
    expectedSelections: [
      ["Hello,", "Hello, World!"],
      ["Hello,", "Hello, World!"],
      ["Hello,", "Hello, World!"],
    ]
  )
}

private func testSelectionRange(
  markedSource: String,
  expectedSelections: [String],
  checkNumberOfSelectionsMatchesExactly: Bool = false,
  sourceLocation: SourceLocation = #_sourceLocation,
) async throws {
  try await testSelectionRange(
    markedSource: markedSource,
    expectedSelections: [expectedSelections],
    checkNumberOfSelectionsMatchesExactly: checkNumberOfSelectionsMatchesExactly,
    sourceLocation: sourceLocation
  )
}

func testSelectionRange(
  markedSource: String,
  expectedSelections: [[String]],
  checkNumberOfSelectionsMatchesExactly: Bool = false,
  sourceLocation: SourceLocation = #_sourceLocation,
) async throws {
  let (documentPositions, text) = DocumentPositions.extract(from: markedSource)

  // check that all expectedSelections are valid
  try #require(
    expectedSelections.count == documentPositions.allMarkers.count,
    "The number of markers and expected selections differ: \(documentPositions.allMarkers.count) markers vs \(expectedSelections.count) selections",
    sourceLocation: sourceLocation
  )
  let flatMappedSelections = expectedSelections.flatMap { $0 }
  try #require(
    flatMappedSelections.allSatisfy { text.contains($0) },
    "The following expected selections are not contained in the source:\n \(flatMappedSelections.filter { !text.contains($0) }.joined(separator: "\n"))",
    sourceLocation: sourceLocation
  )

  // check the actual returned ranges
  let testClient = try await TestSourceKitLSPClient()
  let uri = DocumentURI(for: .swift)
  testClient.openDocument(text, uri: uri)

  let positions: [Position] = documentPositions.allMarkers.map { documentPositions[$0] }

  let request = SelectionRangeRequest(textDocument: TextDocumentIdentifier(uri), positions: positions)
  let response: SelectionRangeRequest.Response = try await testClient.send(request)

  let lineTable = LineTable(text)

  for (positionIndex, range) in response.enumerated() {
    let expected = expectedSelections[positionIndex]

    var numberOfSelectionsReturned = 0
    var currentRange = range
    while true {
      numberOfSelectionsReturned += 1
      guard let parent = currentRange.parent else {
        break
      }
      currentRange = parent
    }

    if checkNumberOfSelectionsMatchesExactly {
      #expect(numberOfSelectionsReturned == expected.count, sourceLocation: sourceLocation)
    } else {
      #expect(numberOfSelectionsReturned >= expected.count, sourceLocation: sourceLocation)
    }

    var rangeIndex = 0
    currentRange = range
    while rangeIndex < expected.count {
      let selectString = getStringOfSelectionRange(lineTable: lineTable, selectionRange: currentRange)
      #expect(selectString == expected[rangeIndex], sourceLocation: sourceLocation)

      guard let parent = currentRange.parent else {
        break
      }
      currentRange = parent
      rangeIndex += 1
    }
  }
}

private func getStringOfSelectionRange(lineTable: LineTable, selectionRange: SelectionRange) -> Substring {
  let lowerBoundOffset = lineTable.utf8OffsetOf(
    line: selectionRange.range.lowerBound.line,
    utf16Column: selectionRange.range.lowerBound.utf16index
  )
  let upperBoundOffset = lineTable.utf8OffsetOf(
    line: selectionRange.range.upperBound.line,
    utf16Column: selectionRange.range.upperBound.utf16index
  )

  let lowerBoundIndex = lineTable.content.index(lineTable.content.startIndex, offsetBy: lowerBoundOffset)
  let upperBoundIndex = lineTable.content.index(lineTable.content.startIndex, offsetBy: upperBoundOffset)
  return lineTable.content[lowerBoundIndex..<upperBoundIndex]
}
