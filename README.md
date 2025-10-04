# SVG Party
Using Clojure to make cool vector art

## To generate & export svgs:
- Start a Project REPL and Connect (aka Jack-in)
- choose deps.edn

- Eval:
(->> {your shape}           
     (adapt/all-as-svg)              
     (svg/serialize)                 
     (spit "my-svg.svg")) 
