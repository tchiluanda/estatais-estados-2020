# estatais-estados-2020
 Versão 2020 da história das Empresas dos Estados

https://github.com/topojson/topojson-server/blob/master/README.md#topology

```zsh
/Users/tchiluanda/node_modules/topojson-server/bin/geo2topo mapa-setores.geojson > mapa.json
```

com 4 digitos ficou 1.5Mb, com 2, 3.1Mb


### Mapas

https://medium.com/@mbostock/command-line-cartography-part-1-897aa8f8ca2c

https://brew.sh/

Baixei o mapa com geobr. Manipulei no R.

O mapa estava muito grande, aí simplifiquei com

```r
mapa <- st_simplify(mapa, dTolerance = .1)
```

Bem interessante brincar com esse dTolerance!

Aí salvei em SHP com

```r
st_write(mapa_qde_export, "./dados/mapa-setores/mapa-setores.shp")
```

Aí usei `shapefile` do node para converter.

```bash
npm install -g shapefile
shp2json mapa-setores.shp -o mapa-setores.json
```

We could now display this in a browser using D3, but first we should apply a geographic projection. By avoiding expensive trigonometric operations at runtime, the resulting GeoJSON renders much faster, especially on mobile devices. Pre-projecting also improves the efficacy of simplification, which we’ll cover in part 3. To install d3-geo-projection’s command-line interface:

```bash
npm install -g d3-geo-projection
```

Precisava descobrir os limites extremos do Brasil em termos de paralelos, aí usei isso aqui, no olho:

```r
ggplot(mapa) + 
  geom_sf() + 
  geom_hline(yintercept = 5.3, color = "red") + 
  geom_hline(yintercept = -33.8, color = "red")
```

O comando do geoproject então vai ficar

```bash
geoproject 'd3.geoConicEqualArea().parallels([-33.8, 5.3]).rotate([120, 0]).fitSize([960, 960], d)' < mapa-setores.json > mp-set-conic.json
```

This d3.geoConicEqualArea projection is California Albers, and as its name suggests, is appropriate for showing California. It’s also equal-area, which is strongly recommended for choropleth maps as the projection will not distort the data. If you’re not sure what projection to use, try d3-stateplane or search spatialreference.org.

The projection you specify to geoproject is an arbitrary JavaScript expression. That means that you can use projection.fitSize to fit the input geometry (represented by d) to the desired 960×960 bounding box!
To preview the projected geometry, use d3-geo-projection’s geo2svg:

```bash
geo2svg -w 960 -h 960 < mp-set-conic.json > mapa.svg
```


https://medium.com/@mbostock/command-line-cartography-part-2-c3a82c5c0f3

Often the data we find online isn’t precisely what we want. Perhaps we need to join multiple data sources, or to convert formats (say from fixed-width text to CSV), or even just to drop unneeded fields to produce smaller files that are faster to download.

You can always write scripts to transform data in arbitrary ways, but writing (and debugging) scripts can become tedious as transformations become more complex. What we want is an iterative, exploratory approach, where at every step we can see what the data looks like; by inspecting as we go, we can fix mistakes as we make them, before they get buried in complexity. And when we’re done, we want to capture our workflow as machine-readable documentation that can be easily reproduced.

The command line is great for this. UNIX has a well-established philosophy of small, decoupled tools that allow powerful manipulation of data. To leverage the power of the command line, we simply need to convert our data into a format that fits a UNIX convention: lines of text. And since JSON is already text, we just need each line of text be a valid JSON value.

Enter newline-delimited JSON (NDJSON), which is simply JSON values separated by newlines (\n). NDJSON combines the best of both worlds: the convenience of the command line for working with files, and the power of JavaScript and its myriad open-source modules. My ndjson-cli module provides tools for converting JSON to NDJSON, for manipulating NDJSON streams (filtering, mapping, joining) and more. To install:

```bash
npm install -g ndjson-cli
```

To convert a GeoJSON feature collection to a newline-delimited stream of GeoJSON features, use ndjson-split:

```
ndjson-split 'd.features' \
  < mp-set-conic.json \
  > mp-set-conic.ndjson
```