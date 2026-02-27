# Gaul Neovim Plugin

Syntax highlighting and filetype settings for [Gaul](../../README.md) in Neovim and Vim.

## Install

### lazy.nvim

```lua
{
  'yourusername/gaul-lang',
  config = function(plugin)
    vim.opt.rtp:append(plugin.dir .. '/editors/neovim')
  end,
}
```

### packer

```lua
use {
  'yourusername/gaul-lang',
  rtp = 'editors/neovim',
}
```

### vim-plug

```vim
Plug 'yourusername/gaul-lang', { 'rtp': 'editors/neovim' }
```

### Manual

Clone the repo and add the plugin path to your config:

```lua
vim.opt.rtp:prepend('/path/to/gaul-lang/editors/neovim')
```

## LSP Setup

Build the language server first:

```bash
cd /path/to/gaul-lang
cargo build --release --bin gaul-lsp
```

### nvim-lspconfig

```lua
local lspconfig = require('lspconfig')
local configs = require('lspconfig.configs')

if not configs.gaul_lsp then
  configs.gaul_lsp = {
    default_config = {
      cmd = { 'gaul-lsp' },  -- or full path to the binary
      filetypes = { 'gaul' },
      root_dir = lspconfig.util.root_pattern('.git', '.gaul-keywords.json'),
    },
  }
end

lspconfig.gaul_lsp.setup({})
```

### Native LSP (Neovim 0.11+)

```lua
vim.lsp.config('gaul', {
  cmd = { 'gaul-lsp' },
  filetypes = { 'gaul' },
  root_markers = { '.git', '.gaul-keywords.json' },
})

vim.lsp.enable('gaul')
```

### Inlay Hints

Enable parameter name hints at call sites (Neovim 0.10+):

```lua
vim.lsp.inlay_hint.enable(true)
```

## What You Get

The plugin provides:
- Filetype detection for `.gaul` files
- Syntax highlighting for keywords, strings, numbers, comments, operators
- `commentstring` set to `// %s` (works with gcc, commentary.nvim, etc.)
- Proper indent settings (2-space tabs)
- `suffixesadd` so `gf` on import paths works

Once the LSP attaches, semantic tokens override the base syntax highlighting. This means localized keywords (via custom keyword files) get proper coloring through the LSP, even though the syntax file only knows the default English keywords.
