# weedDisp

Project to publish my Master's thesis on modeling weed dispersal in agriculture.

The objective of this repository is to make the paper reproducible from data to manuscript by using automated and version control system. This repo is based in the template that was kindly shared by Timothée Poisot [here][tp].

The paper is under development. If you want to take a look in the progress, the manuscript pdf is [here][pdf].
And if you can:

## Feel free to contribute.
Comments and suggestions can be made by pull request on the [`manuscript.Rmd`][ms] file by using the [Critic Markup][cm] render with the following commands:

This is the original sentence.
this is {-- the --} original sentence. #remove "the"
this is the {++ final ++} original sentence. #add "final"
this is {~~ the ~> a ~~} replace "the" by "a"
this is {== the ==}{>> this word shouldn't be here <<} original sentence. #comment on the word "the"

You can also contribute by commenting on the [pdf][pdf] or [odt][odt] file.

<!-- links -->
[tp]: https://github.com/PoisotLab/PLMT
[ms]: https://github.com/willvieira/weedDist/master/manuscript.Rmd
[cm]: http://criticmarkup.com/
[pdf]: https://github.com/willvieira/weedDist/blob/master/output/manuscript_preprint.pdf
[odt]: https://github.com/willvieira/weedDist/blob/master/output/manuscript.odt
