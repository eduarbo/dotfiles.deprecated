au BufNewFile,BufRead riemann.config set filetype=clojure
au BufNewFile,BufRead *.cljs set filetype=clojurescript
au BufNewFile,BufRead *.less set filetype=less
au BufNewFile,BufRead *.scss set filetype=scss
au BufNewFile,BufRead *.hbs set filetype=handlebars
au BufNewFile,BufRead *.fish set filetype=fish
au BufNewFile,BufRead *.hamlc set filetype=haml
au BufNewFile,BufRead *.es6 set filetype=javascript
au BufNewFile,BufRead *jshintrc set filetype=json
au BufNewFile,BufRead *.{md,mdown,mkd,mkdn,markdown,mdwn} set filetype=markdown
au BufNewFile,BufRead *.sql.pre,*.sql.post set filetype=sql
au BufNewFile,BufRead Vagrantfile,Capfile set filetype=ruby

" Hacks to make them work with handlebars-mustache plugin
au BufNewFile,BufRead *.html set syntax=mustache " I tried setting syntax in ftplugin/html/html.vim but didn't work :(
au BufNewFile,BufRead *.mustache,*.hogan,*.hulk,*.hjs set filetype=mustache syntax=mustache
au BufNewFile,BufRead *.handlebars,*.hbs set filetype=handlebars syntax=mustache

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
