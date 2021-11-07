elm make src/Main.elm --optimize --output=dist/simplication.js
uglifyjs dist/simplication.js --compress 'pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe' | uglifyjs --mangle --output dist/simplification.0.1.0.min.js
rm dist/simplication.js
cp node_modules/viz.js/viz.js dist/viz.js
cp node_modules/viz.js/full.render.js dist/full.render.js