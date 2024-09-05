call plug#begin()
	Plug 'scrooloose/nerdtree'
	Plug 'prabirshrestha/vim-lsp'
	Plug 'tpope/vim-fugitive'
	Plug 'sheerun/vim-polyglot'
	Plug 'maxboisvert/vim-simple-complete'
	Plug 'tpope/vim-commentary'
	Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
	Plug 'junegunn/fzf.vim'
	Plug 'jiangmiao/auto-pairs'
	Plug 'tribela/vim-transparent'
	Plug 'morhetz/gruvbox'
	Plug 'catppuccin/vim', { 'as': 'catppuccin'  }
	Plug 'vim-airline/vim-airline'
	Plug 'vim-airline/vim-airline-themes'
call plug#end()

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
inoremap <C-K> <Esc>lDa
inoremap <C-Y> <Esc>Pa
inoremap <C-X><C-S> <Esc>:w<CR>a

" Keymaps for NERDTree
nnoremap <leader>op :NERDTreeToggle<CR>
nnoremap <leader>e :NERDTreeFocus<CR>

" Keymaps for Fuzzy
nnoremap <leader>ff :FZF<CR>
nnoremap <leader><leader> :FZF<CR>

" Lightline
set laststatus=2
set noshowmode
let g:airline_theme = 'catppuccin_mocha'

" Snippets
let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<c-b>"
let g:UltiSnipsJumpBackwardTrigger="<c-z>"

" If you want :UltiSnipsEdit to split your window.
let g:UltiSnipsEditSplit="vertical"
set clipboard+=unnamedplus
