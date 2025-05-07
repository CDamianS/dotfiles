call plug#begin()
	Plug 'scrooloose/nerdtree'
	Plug 'prabirshrestha/vim-lsp'
	Plug 'tpope/vim-fugitive'
	Plug 'sheerun/vim-polyglot'
	Plug 'maxboisvert/vim-simple-complete'
	Plug 'tpope/vim-commentary'
	Plug 'jiangmiao/auto-pairs'
	Plug 'tribela/vim-transparent'
	Plug 'catppuccin/vim', { 'as': 'catppuccin'  }
	Plug 'leafo/moonscript-vim'
	Plug 'kchmck/vim-coffee-script'
call plug#end()

packadd! CtrlP

" Colorscheme
set termguicolors
" set background=dark
colorscheme catppuccin_mocha

" Number Config
set number
set rnu

" Keymaps
let mapleader=" "
nnoremap <leader>fs :w<CR>
nnoremap <leader>w <C-w>
nnoremap <leader>wq :q<CR>
nnoremap <leader>wQ :q!<CR>
inoremap jk <Esc>

" Mimic Emacs Line Editing in Insert Mode Only
inoremap <C-A> <Home>
inoremap <C-B> <Left>
inoremap <C-E> <End>
inoremap <C-F> <Right>

" Keymaps for NERDTree
nnoremap <leader>op :NERDTreeToggle<CR>
nnoremap <leader>e :NERDTreeFocus<CR>

" Airline
set laststatus=2
set noshowmode
let g:airline_theme = 'catppuccin_mocha'
