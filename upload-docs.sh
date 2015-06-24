standalone-haddock --package-db /home/ricky/.ghc/x86_64-linux-7.10.1/package.conf.d/ -o doc . --hyperlink-source
echo '<!doctype html>' > doc/index.html
echo '<html><head><meta http-equiv="refresh" content="0; url=./pagure/" /></head></html>' >> doc/index.html
rsync --delete --partial --progress -avzre ssh doc/ fedorapeople.org:public_html/pagure-haskell
rm -rf doc
