import XCTest
import SwiftTreeSitter
import TreeSitterGilear

final class TreeSitterGilearTests: XCTestCase {
    func testCanLoadGrammar() throws {
        let parser = Parser()
        let language = Language(language: tree_sitter_gilear())
        XCTAssertNoThrow(try parser.setLanguage(language),
                         "Error loading Gilear grammar")
    }
}
