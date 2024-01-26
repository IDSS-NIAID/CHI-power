### Notes

* Use [DisGeNET](https://www.disgenet.org/) information to categorize analytes. Use the [disgenet2r](https://www.disgenet.org/static/disgenet2r/disgenet2r.html) package.
    * This is what SomaLogic used on their [11K assay infosheet](https://somalogic.com/somascan-11k-assay/).
    * It uses information from [PANTHER](https://www.pantherdb.org/).
* The [SomaLogic menu](https://menu.somalogic.com/) has all analytes in the 7K and 11K panels.
* To deploy a serverless app (where `shiny` is the directory containing the app, and `site` is where the static site will be generated):

`shinylive::export('shiny', 'site')`

* This could also be served up and tested locally with:

`httpuv::runStaticServer('site/')`

