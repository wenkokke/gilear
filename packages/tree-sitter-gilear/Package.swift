// swift-tools-version:5.3
import PackageDescription

let package = Package(
    name: "TreeSitterGilear",
    products: [
        .library(name: "TreeSitterGilear", targets: ["TreeSitterGilear"]),
    ],
    dependencies: [
        .package(url: "https://github.com/ChimeHQ/SwiftTreeSitter", from: "0.8.0"),
    ],
    targets: [
        .target(
            name: "TreeSitterGilear",
            dependencies: [],
            path: ".",
            sources: [
                "src/parser.c",
                // NOTE: if your language has an external scanner, add it here.
            ],
            resources: [
                .copy("queries")
            ],
            publicHeadersPath: "bindings/swift",
            cSettings: [.headerSearchPath("src")]
        ),
        .testTarget(
            name: "TreeSitterGilearTests",
            dependencies: [
                "SwiftTreeSitter",
                "TreeSitterGilear",
            ],
            path: "bindings/swift/TreeSitterGilearTests"
        )
    ],
    cLanguageStandard: .c11
)
