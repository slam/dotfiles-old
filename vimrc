
let uname = substitute(system("uname"),"\n","","g")

let mapleader = "," " change from \ to ,

" Quickly edit/reload the vimrc file
nmap <silent> <leader>ev :e $MYVIMRC<CR>
nmap <silent> <leader>sv :so $MYVIMRC<CR>

"
" Example originally for pathogen. Leave it around since it's
" generally useful.
"
" Example module management with git:
"
" $ git submodule add http://github.com/scrooloose/nerdtree.git bundle/nerdtree
" $ git submodule init
" $ git commit -m 'add nerd tree plugin as submodule'
"
" To pull into another machine:
"
" $ git submodule update --init
"

"
" Switched from pathogen to vundle
"
" To pull bundles into a new machine, run in bash:
"
" git clone http://github.com/gmarik/vundle.git ~/.vim/bundle/vundle
"
" And in vim:
"
" :BundleInstall
"
filetype off
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

Bundle 'gmarik/vundle'

Bundle 'tpope/vim-fugitive'
Bundle 'fs111/pydoc.vim'
Bundle 'vimwiki'
Bundle 'ack.vim'
Bundle 'nelstrom/vim-mac-classic-theme'
Bundle 'msanders/snipmate.vim'
Bundle 'matchit.zip'
Bundle 'a.vim'
Bundle 'bufexplorer.zip'
Bundle 'vcscommand.vim'
Bundle 'actionscript.vim--Jethani'
Bundle 'altercation/vim-colors-solarized'
Bundle 'unimpaired.vim'

" Command-T requires manual setup:
"
" cd ~/.vim/bundle/Command-T/ruby/command-t; ruby extconf.rb; make
Bundle 'Command-T'
let g:CommandTMatchWindowReverse=1
let g:CommandTMaxFiles=50000

"
" Color Schemes
"
if has("gui_running")
  set background=light
  set guifont=Droid\ Sans\ Mono:h13
  colorscheme solarized
  syntax on
else
  if &term =~ "xterm"
    set t_Co=8
    set t_Sf=[3%p1%dm
    set t_Sb=[4%p1%dm
  endif

  if &term =~ "screen*" || &term =~ "xterm-256" || uname == "Darwin"
    set title
    set t_Co=256
    set t_ts=]2;
    set t_fs=
  endif

  if &t_Co > 2
    set background=light
    syntax on
  endif

  if &t_Co >= 256
    "colorscheme mac_classic
    let g:solarized_termcolors=16
    colorscheme solarized
  endif
endif

"
" Basic Stuff
"
set nocompatible " explicitly get out of vi-compatible mode
set noexrc " don't load vimrc found in current directory
set backspace=indent,eol,start " make backspace flexible
set hidden " change buffer without saving
set noerrorbells " don't make noise
set novisualbell " dont' blink
set nobackup " don't make a backup before overwriting a file
set wildmenu " turn on command line completion
set wildmode=list:longest " list all matches until last one standing
set wildignore+=*.class,*.o,*.obj,*.d,*.hh,*.png,*.swf,*.gif,*.jpg,*.fla,*.mp3,*.jar " ignore these files
set laststatus=2 " show status line even with one window left
set list " show tabs and trailing spaces
set listchars=tab:>-,trail:-
set ruler " show the cursor position all the time
set showmatch " jump to matching bracket briefly
"set title " set title to the value of titlestring
set modelines& " restore modelines to 5 on OS X
if has("win32") || has("win64")
  set directory=$TMP
else
  set directory=~/tmp//,/var/tmp//,/tmp//
endif

if v:version >= 701

elseif v:version >= 700

elseif v:version >= 600

endif

"
" GUI-related Stuff
"
if has("gui_running")
  set guioptions-=T " include toolbar
  set guioptions-=m " include menubar
  set guioptions-=l " exclude scrollbars
  set guioptions-=r " exclude scrollbars
  set guioptions-=L " exclude scrollbars
  set guioptions-=R " exclude scrollbars
  set mouse=a
  set clipboard=autoselect
else
  set clipboard=exclude:screen.*
endif

"
" Indenting behaviors
"
set autoindent
set smarttab
set smartindent
filetype plugin indent on

set pastetoggle=<leader>pt
set textwidth=78
set statusline=%<%f\ %y%m%r%=%-14.(%l,%c%V%)\ %P " add filetype to status line

set viminfo='20,\"50 " don't store more than 50 lines of registers
set history=50 " keep 50 lines of command line history

" search tags from cwd, then from directory of the current file upwards until found
set tags=tags,./tags;/,$HOME/commontags

"
" Macros
"

" Edit another file in the same directory.
if has("unix")
  map <leader>e :e <C-R>=expand("%:p:h") . "/" <CR>
else
  map <leader>e :e <C-R>=expand("%:p:h") . "\\" <CR>
endif


nn <leader>d :bd<CR>
nn <leader>c <C-W>c

"
" Plugins
"
map <leader>b :BufExplorer<cr>
map <leader>s :SBufExplorer<cr>
map <leader>v :VSBufExplorer<cr>

" Fix a color bug with vim and screen. If vim has set a background color the
" console still use the color after vim quits.
if !has("gui_running")
  augroup fix_gnu_screen
    autocmd!
    autocmd VimLeave * :set term=screen
  augroup END
endif

if has("cscope")
  " Use quick fix window
  set cscopequickfix=s-,c-,d-,i-,t-,e-
endif

autocmd BufNewFile,BufRead *.as set filetype=actionscript
"autocmd FileType php set shiftwidth=2 expandtab tabstop=2
autocmd FileType php set shiftwidth=4 noexpandtab tabstop=4
autocmd FileType actionscript set shiftwidth=4 noexpandtab tabstop=4
autocmd FileType python set shiftwidth=4 expandtab tabstop=4
autocmd FileType java set shiftwidth=4 expandtab tabstop=4
autocmd FileType perl set shiftwidth=2 expandtab tabstop=2
autocmd FileType javascript set shiftwidth=2 expandtab tabstop=2
autocmd FileType html set shiftwidth=2 expandtab tabstop=2
autocmd FileType haskell set shiftwidth=4 expandtab tabstop=4
autocmd FileType sh set shiftwidth=2 expandtab tabstop=2
autocmd FileType c set nolist
" See :help last-position-jump
autocmd BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g`\"" | endif

autocmd User fugitive
  \ if fugitive#buffer().type() =~# '^\%(tree\|blob\)$' |
  \   nnoremap <buffer> .. :edit %:h<CR> |
  \ endif
autocmd BufReadPost fugitive://* set bufhidden=delete

set statusline=%<%f\ %h%m%r%{fugitive#statusline()}%=%-14.(%l,%c%V%)\ %P

"
" Source in local vimrc if exists.
"
let localvimrc=expand("~/.vim/vimrc.local")
if filereadable(localvimrc)
  exec "source " . localvimrc
endif

set nobackup
set nowritebackup
if exists("&undofile")
  set undofile
  set undodir=$HOME/.vim_undo,/tmp
endif

let g:ackprg="parallel -u -k -j +0 -m -n5 ack --type-add php=tpl -H --nocolor --nogroup --column $* ::: *"

function! SearchSource(prog)
  let s:wordUnderCursor = expand("<cword>")
  let s:cmd = a:prog . " " . s:wordUnderCursor
  execute s:cmd
  botright copen
endfunction

map <leader>g :call SearchSource('Ack')<cr>
map <leader>G :call SearchSource('Ggrep')<cr>

if has("cscope")
  set csto=0
  set cst
  set nocsverb
  " add any database in current directory
  if filereadable("cscope.out")
    cs add cscope.out
  " else add database pointed to by environment
  elseif $CSCOPE_DB != ""
    cs add $CSCOPE_DB
  endif
  set csverb

  set cscopequickfix=s-,c-,d-,i-,t-,e-

  nmap <C-\>s :cs find s <C-R>=expand("<cword>")<CR><CR>
  nmap <C-\>g :cs find g <C-R>=expand("<cword>")<CR><CR>
  nmap <C-\>c :cs find c <C-R>=expand("<cword>")<CR><CR>
  nmap <C-\>t :cs find t <C-R>=expand("<cword>")<CR><CR>
  nmap <C-\>e :cs find e <C-R>=expand("<cword>")<CR><CR>
  nmap <C-\>f :cs find f <C-R>=expand("<cfile>")<CR><CR>
  nmap <C-\>i :cs find i ^<C-R>=expand("<cfile>")<CR>$<CR>
  nmap <C-\>d :cs find d <C-R>=expand("<cword>")<CR><CR>

  " Using 'CTRL-spacebar' then a search type makes the vim window
  " split horizontally, with search result displayed in
  " the new window.

  nmap <C-@>s :scs find s <C-R>=expand("<cword>")<CR><CR>
  nmap <C-@>g :scs find g <C-R>=expand("<cword>")<CR><CR>
  nmap <C-@>c :scs find c <C-R>=expand("<cword>")<CR><CR>
  nmap <C-@>t :scs find t <C-R>=expand("<cword>")<CR><CR>
  nmap <C-@>e :scs find e <C-R>=expand("<cword>")<CR><CR>
  nmap <C-@>f :scs find f <C-R>=expand("<cfile>")<CR><CR>
  nmap <C-@>i :scs find i ^<C-R>=expand("<cfile>")<CR>$<CR>
  nmap <C-@>d :scs find d <C-R>=expand("<cword>")<CR><CR>

  " Hitting CTRL-space *twice* before the search type does a vertical
  " split instead of a horizontal one

  nmap <C-@><C-@>s
    \:vert scs find s <C-R>=expand("<cword>")<CR><CR>
  nmap <C-@><C-@>g
    \:vert scs find g <C-R>=expand("<cword>")<CR><CR>
  nmap <C-@><C-@>c
    \:vert scs find c <C-R>=expand("<cword>")<CR><CR>
  nmap <C-@><C-@>t
    \:vert scs find t <C-R>=expand("<cword>")<CR><CR>
  nmap <C-@><C-@>e
    \:vert scs find e <C-R>=expand("<cword>")<CR><CR>
  nmap <C-@><C-@>i
    \:vert scs find i ^<C-R>=expand("<cfile>")<CR>$<CR>
  nmap <C-@><C-@>d
    \:vert scs find d <C-R>=expand("<cword>")<CR><CR>

endif

" vim:tw=78:ts=2:et:sw=2

