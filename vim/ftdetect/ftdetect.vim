au BufNewFile,BufRead riemann.config set filetype=clojure
au BufNewFile,BufRead *.cljs set filetype=clojurescript
au BufNewFile,BufRead *.fish set filetype=fish
au BufNewFile,BufRead *.hamlc set filetype=haml
au BufNewFile,BufRead *.es6 set filetype=javascript
au BufNewFile,BufRead *jshintrc set filetype=json
au BufNewFile,BufRead *.{md,mdown,mkd,mkdn,markdown,mdwn} set filetype=markdown
au BufNewFile,BufRead *.sql.pre,*.sql.post set filetype=sql
au BufNewFile,BufRead Vagrantfile,Capfile set filetype=ruby

au BufNewFile,BufRead /etc/nginx/conf/*                      setlocal ft=nginx
au BufNewFile,BufRead /etc/nginx/sites-available/*           setlocal ft=nginx
au BufNewFile,BufRead /usr/local/etc/nginx/sites-available/* setlocal ft=nginx
au BufNewFile,BufRead vhost.nginx                            setlocal ft=nginx

" Django" {{{1
au BufNewFile,BufRead admin.py     set filetype=python.django
au BufNewFile,BufRead urls.py      set filetype=python.django
au BufNewFile,BufRead models.py    set filetype=python.django
au BufNewFile,BufRead views.py     set filetype=python.django
au BufNewFile,BufRead settings.py  set filetype=python.django
au BufNewFile,BufRead forms.py     set filetype=python.django
au BufNewFile,BufRead common_settings.py  set filetype=python.django
"}}}

au BufWinEnter *.txt if &ft == 'help' | wincmd L | endif
" Reload vimrc
au BufWritePost vimrc so %
