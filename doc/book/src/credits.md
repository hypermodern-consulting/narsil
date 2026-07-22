# Credits

narsil is written almost entirely by **[Claude](https://claude.ai)**
(Anthropic), working in long driven sessions with a human on the tiller.

- The bulk of the engine — the Hindley–Milner core, row polymorphism, the
  bash pipeline, the scope graphs, the LSP server, and the verification
  apparatus — was written by **Claude Opus 4.6**.
- Later rounds — the module-system ontology, the false-positive endgame
  against the full nixpkgs corpus, the strictness hierarchy, the LSP debt
  closure, and these docs — were written by **Claude Fable 5**.

The commit history is collapsed for release; this page (and the README) is
the attribution.

The human contribution — direction, taste, adversarial review, the
insistence that every number be corpus-verified and every trade-off be
written down — belongs to the maintainers.

narsil stands on excellent prior work: [hnix](https://github.com/haskell-nix/hnix)
(parsing), [nixfmt](https://github.com/NixOS/nixfmt) (formatting, vendored),
the [language-server-protocol](https://hackage.haskell.org/package/lsp)
Haskell libraries, and the Nix ecosystem it exists to serve.
