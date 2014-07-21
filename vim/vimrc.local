scriptencoding utf-8

runtime macros/matchit.vim

set runtimepath+=~/.vim/bundle/vundle/
call vundle#rc()

Plugin 'gmarik/vundle'

" github
Plugin 'ervandew/supertab'
Plugin 'godlygeek/tabular'
Plugin 'rking/ag.vim'
Plugin 'nathanaelkane/vim-indent-guides'
Plugin 'scrooloose/nerdtree'
Plugin 'kien/ctrlp.vim'
Plugin 'sjbach/lusty'
Plugin 'sjl/gundo.vim'
Plugin 'jlanzarotta/bufexplorer'
Plugin 'tpope/vim-commentary'
Plugin 'tpope/vim-abolish'
Plugin 'tpope/vim-fugitive'
Plugin 'tpope/vim-repeat'
Plugin 'tpope/vim-surround'
Plugin 'bling/vim-airline'

" snipmate
Plugin 'MarcWeber/vim-addon-mw-utils'
Plugin 'tomtom/tlib_vim'
Plugin 'garbas/vim-snipmate'

" vim-scripts
Plugin 'bufkill.vim'

" {{{ useful options
set autoindent
set cinoptions=:0,l1,g0,t0,(0
set confirm
set dict+=/usr/share/dict/words
set expandtab
set gdefault
set hidden
set hlsearch
set ignorecase smartcase
set incsearch
set laststatus=2
set list listchars=tab:»\ ,trail:·
set modeline
set nocursorline
set nofoldenable
set nowrap
set number
set pastetoggle=<Leader>+
set path+=.;,./**
set scrolloff=5
set shiftwidth=4 tabstop=4
set showbreak=…
set showcmd
set showmatch
set spelllang=en_us
set switchbuf=useopen,usetab
set tags+=/
set timeoutlen=500
set virtualedit=block
set visualbell
set whichwrap+=[,]
set wildignore=*.swp,*.bak,*.pyc,*.class,*.o,*.obj
set wildmode=list:longest

" VIM 7.3 specific options
if version >= 703
	set cryptmethod=blowfish

	" persistent undo
	set undodir=~/.vim/undodir
	set undofile
endif

au BufRead,BufNewFile * let b:args=''

let python_highlight_all=1

" airline
let g:airline#extensions#tabline#enabled = 1
let g:airline_powerline_fonts = 1

" gundo
let g:gundo_right = 1

" taglist
let Tlist_Use_Right_Window = 1
let Tlist_WinWidth         = 50

" snipmate
let g:snips_author = 'Dror Levin'

" netrw
let g:netrw_browse_split = 3
let g:netrw_list_hide    = '\(^\|\s\s\)\zs\.\S\+'

" BufExplorer
let g:bufExplorerSortBy = 'name'

" OmniCppComplete
let OmniCpp_NamespaceSearch     = 1
let OmniCpp_GlobalScopeSearch   = 1
let OmniCpp_ShowAccess          = 1
let OmniCpp_ShowPrototypeInAbbr = 1 " show function parameters
let OmniCpp_MayCompleteDot      = 1 " autocomplete after .
let OmniCpp_MayCompleteArrow    = 1 " autocomplete after ->
let OmniCpp_MayCompleteScope    = 0 " autocomplete after ::
let OmniCpp_DefaultNamespaces   = ["std", "_GLIBCXX_STD"]

" gnupg
let g:GPGPreferSign = 1
" }}}

" {{{ functions
func! GentooCompare(i1, i2)
	let s1 = substitute(a:i1, '^[-+~]', '', '')
	let s2 = substitute(a:i2, '^[-+~]', '', '')
	return l:s1 == l:s2 ? 0 : l:s1 > l:s2 ? 1 : -1
endfunc

let b:updatetime_users = 0
au BufRead,BufNewFile * let b:updatetime_users = 0
func! Taglist_Toggle()
	TlistToggle
	if exists('#taglist')
		au! taglist
		augroup! taglist
		let b:updatetime_users -= 1
		if b:updatetime_users <= 0
			setl updatetime=4000
		endif
	else
		let b:updatetime_users += 1
		setl updatetime=500
		augroup taglist
			au!
			au CursorHold * TlistHighlightTag
		augroup end
	endif
endfunc

nnoremap <silent> <Leader>/ :if Auto_Highlight_Toggle() \| setl hls \| endif<CR>
func! Auto_Highlight_Toggle()
	let @/=''
	if exists('#auto_highlight')
		au! auto_highlight
		augroup! auto_highlight
		let b:updatetime_users -= 1
		if b:updatetime_users <= 0
			setl updatetime=4000
		endif
		return 0
	else
		let b:updatetime_users += 1
		setl updatetime=500
		augroup auto_highlight
			au!
			au CursorHold * let @/='\<'.expand('<cword>').'\>'
		augroup end
		return 1
	endif
endfunc

command! DiffOrig vert new | set bt=nofile | r # | 0d_ | diffthis | wincmd p | diffthis

command! Q q

" }}}

" {{{ mappings
inoremap <M-Left> <C-o>b
inoremap <M-Right> <C-o>w
inoremap <M-C-Left> <C-o>B
inoremap <M-C-Right> <C-o>W

inoremap <C-Del> <C-o>dw
inoremap <C-BS> <Esc>dbxa

" move when soft wrapping and horizontally
nnoremap <C-h> zH
nnoremap <C-j> gj
nnoremap <C-k> gk
nnoremap <C-l> zL
vnoremap <C-h> zH
vnoremap <C-j> gj
vnoremap <C-k> gk
vnoremap <C-l> zL

" easily move through windows
nnoremap <M-Right> <C-w>l
nnoremap <M-Left> <C-w>h
nnoremap <M-Up> <C-w>k
nnoremap <M-Down> <C-w>j

" edit in working directory
noremap <Leader>ew :e <C-r>=substitute(expand("%:p:h"), ' ', '\\\ ', 'g')<CR>/
noremap <Leader>es :sp <C-r>=substitute(expand("%:p:h"), ' ', '\\\ ', 'g')<CR>/
noremap <Leader>ev :vsp <C-r>=substitute(expand("%:p:h"), ' ', '\\\ ', 'g')<CR>/
noremap <Leader>et :tabe <C-r>=substitute(expand("%:p:h"), ' ', '\\\ ', 'g')<CR>/

nmap <silent> <Leader>r :source $MYVIMRC<CR>

nnoremap <silent> <Tab> :bnext<CR>
nnoremap <silent> <S-Tab> :bprevious<CR>
noremap <C-Left> :tabprevious<CR>
noremap <C-Right> :tabnext<CR>
noremap <C-n> :tabnew<CR>
noremap <BS> O<Esc>
noremap <CR> o<Esc>
noremap Y y$
nnoremap <silent> - :let @/=''<CR>
nnoremap <silent> _ :setl spell!<CR>
vnoremap < <gv
vnoremap > >gv
nnoremap <silent> <Leader>- :setl wrap! linebreak! list!<CR>
inoremap <C-y> <C-y><C-o>:pclose<CR>
inoremap <C-e> <C-e><C-o>:pclose<CR>
nnoremap <Space> za
nnoremap <silent> <Leader><Space> :setl foldenable!<CR>
noremap N Nzz
noremap n nzz

" remarkably, Nul is acatually C-Space
imap <Nul> <C-x><C-o>
imap <C-Space> <C-x><C-o>

" ctags
map <C-\> :tab split<CR>:exec("tag ".expand("<cword>"))<CR>
map <M-]> :vsp<CR>:exec("tag ".expand("<cword>"))<CR>

"nnoremap <Leader>! :setl nolist<CR>:ConqueTerm bash -l<CR>
"nnoremap <Leader>@ :setl nolist<CR>:ConqueTerm ipython<CR>

vnoremap <F2> s<Esc>:execute 'normal a' . join(sort(split(getreg('"')), 'GentooCompare'), ' ')<CR>gqq
nnoremap <silent> <S-F2> :%s/<C-v><Esc>.\{-}m//ge<CR>:%s/<C-v><C-h>.//ge<CR>''
nnoremap <F3> :cprev<CR>
nnoremap <F4> :cnext<CR>
nnoremap <S-F3> [c
nnoremap <S-F4> ]c
nnoremap <F5> :Gstatus<CR>
nnoremap <F6> :GundoToggle<CR>
nnoremap <Leader>t :NERDTreeToggle<CR>
nnoremap <F7> :NERDTreeToggle<CR>
nnoremap <F8> :call Taglist_Toggle()<CR>
nnoremap <F9> :LustyJuggler<CR>
noremap <Leader>q :quit<CR>
noremap <F10> <Esc>:quit<CR>
noremap <S-F10> <Esc>:quitall!<CR>
noremap <Leader>d :BD<CR>
noremap <M-F10> <Esc>:BD<CR>
noremap <Leader>w :update<CR>
noremap <F11> <Esc>:update<CR>
noremap <S-F11> <Esc>:exit<CR>
noremap <M-F11> <Esc>:wall<CR>
imap <F10> <Esc><F10>
imap <S-F10> <Esc><S-F10>
imap <M-F10> <Esc><M-F10>
imap <F11> <Esc><F11>
imap <S-F11> <Esc><S-F11>
imap <M-F11> <Esc><M-F11>
"}}}

" {{{ set per filetype options
filetype plugin indent on
syntax on

au BufRead,BufNewFile *.txt setlocal wrapmargin=2 textwidth=78 formatoptions+=a spell
au FileType * exec 'setlocal dict='.substitute($VIMRUNTIME, ' ', '\\\ ', 'g').'/syntax/'.&filetype.'.vim,'.&dict
au FileType mail setlocal spell
au FileType rst setlocal noexpandtab wrapmargin=2 textwidth=78 formatoptions+=a spell
au FileType ebuild nnoremap <F6> :silent !repoman manifest<CR><C-l>
au FileType changelog setlocal formatoptions+=a spell
au FileType python setlocal foldmethod=indent formatoptions=wcrq2l include=^\\s*\\(from\\\|import\\)
au FileType vim setlocal foldmethod=marker
au FileType c,cpp setlocal foldmethod=syntax tags+=~/.vim/tags/glib,~/.vim/tags/dbus
au FileType c,cpp nnoremap <F5> :exec '!./%:r ' . b:args<CR>
au FileType c noremap <C-F12> :!ctags -R --sort=foldcase --c-kinds=+pl --fields=+S .<CR>
au FileType cpp noremap <C-F12> :!ctags -R --sort=foldcase --c++-kinds=+pl --fields=+iaS --extra=+q .<CR>
au FileType cpp set tags+=~/.vim/tags/cpp,~/.vim/tags/boost,~/.vim/tags/qt4,~/.vim/tags/kde
au FileType java compiler javac
au FileType java nnoremap <F5> :exec '!java %:r ' . b:args<CR>
au FileType java nnoremap <F6> :make %<CR>
au CursorMovedI,InsertLeave * if pumvisible() == 0|silent! pclose|endif
" }}}

" {{{ colors
"highlight link WhitespaceErrors Error
"au BufRead,BufNewFile * match WhitespaceErrors /\s\+$/

set background=dark
colorscheme Tomorrow-Night-Bright
" }}}

if has('cscope')
	set cscopetag
	if filereadable('cscope.out')
		" add any database in current directory
		cs add cscope.out
	elseif $CSCOPE_DB != ''
		" else add database pointed to by environment
		cs add $CSCOPE_DB
	endif

	nmap <C-\>s :cs find s <C-R>=expand('<cword>')<CR><CR>
	nmap <C-\>g :cs find g <C-R>=expand('<cword>')<CR><CR>
	nmap <C-\>c :cs find c <C-R>=expand('<cword>')<CR><CR>
	nmap <C-\>t :cs find t <C-R>=expand('<cword>')<CR><CR>
	nmap <C-\>e :cs find e <C-R>=expand('<cword>')<CR><CR>
	nmap <C-\>f :cs find f <C-R>=expand('<cfile>')<CR><CR>
	nmap <C-\>i :cs find i ^<C-R>=expand('<cfile>')<CR>$<CR>
	nmap <C-\>d :cs find d <C-R>=expand('<cword>')<CR><CR>
endif
