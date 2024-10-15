package tree_sitter_gilear_test

import (
	"testing"

	tree_sitter "github.com/tree-sitter/go-tree-sitter"
	tree_sitter_gilear "github.com/wenkokke/gilear/bindings/go"
)

func TestCanLoadGrammar(t *testing.T) {
	language := tree_sitter.NewLanguage(tree_sitter_gilear.Language())
	if language == nil {
		t.Errorf("Error loading Gilear grammar")
	}
}
