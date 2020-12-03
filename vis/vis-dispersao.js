const vis = {

    params : {

        refs: {

            svg: "svg.vis-dispersao",
            cont: "div.vis-dispersao",
            data: "./dados/dados_roe.csv"
    
        },
    
        sels: {
    
            svg : null,
            cont : null,
            axis : {}
    
        },
    
        elems: {
    
            svg:  null,
            cont: null
    
        },
    
        dims : {
    
            h: null,
            w: null,
            
            margins: {
    
                top: 10,
                left: 20,
                right: 10,
                bottom: 20
    
            },

            ranges : {

                x : null,

                y : null

            }
    
        }      

    },

    data : {

        raw : null,

        processed : {},

        variables : {

            // proprio de cada projeto
            quantitative : ["lucros", "PL"],

            // proprio de cada projeto
            // para este em particular, vou usar para definir as variaveis que pretendo usar
            // para permitir a filtragem dos dados. e também, no caso do "dep", para permitir 
            // estilizar os pontos usando css. para isso vou usar data-attributes.
            categorical :  ["dep", "gov", "plr_rva", "cat_ROE", "setor", "Nome_estado"]

        },

        //faz mais sentido que o objeto domains contemple todas as variáveis, de qq tipo (em vez de usar um objeto domain para cada tipo de variavel)
        domains : {}

    },

    utils : {

        // "administrative tasks"

        generates_refs: function() {

            vis.params.sels.svg  = d3.select(vis.params.refs.svg);
            vis.params.sels.cont = d3.select(vis.params.refs.cont);

            vis.params.elems.svg  = document.querySelector(vis.params.refs.svg);
            vis.params.elems.cont = document.querySelector(vis.params.refs.cont);

        },

        get_size: function() {

            let win_w = window.getComputedStyle(vis.params.elems.cont).getPropertyValue("width").slice(0,-2); //window.innerWidth;
            let win_h = window.innerHeight;

            win_w = win_w > 680 ? 680 : win_w;

            let pos_vis_y = vis.params.elems.svg.getBoundingClientRect().y;

            vis.params.dims.h = 500;//win_h - pos_vis_y - vis.dims.margins.top - vis.dims.margins.bottom;
            
            vis.params.dims.w = win_w;//+vis.sels.svg.style("width").slice(0, -2);

        },

        set_size: function() {

            vis.params.elems.svg.style.setProperty(
                "height", vis.params.dims.h + "px");

            //vis.elems.svg.style.setProperty("background-color", "coral");


        },

        build_variables_domains : function() {

            vis.data.variables.quantitative.forEach(variable => {
                vis.data.domains[variable] = d3.extent(vis.data.raw, d => +d[variable]);
            })

            vis.data.variables.categorical.forEach(variable => {
                vis.data.domains[variable] = utils.unique(vis.data.raw, variable);
            })

        },

        set_start_domain_zero : function(variable) {

            vis.data.domains[variable][0] = 0;

        },

        update_range : function(dim) {

            const dims = vis.params.dims;

            if (dim == "x") {
                
                vis.params.dims.ranges[dim] = [
                    dims.margins.left,
                    dims.w - dims.margins.right
                ]

            } else if (dim == "y") {

                vis.params.dims.ranges[dim] = [
                    dims.h - dims.margins.bottom,
                    dims.margins.top
                ]

            }

        },

        read_data : function(url) {

            d3.csv(url).then(
                data => vis.control.begin(data)
            );

        }

    },

    render : {

        scales : {

            x : d3.scaleLinear(),

            y : d3.scaleLinear(),

            update :  {

                range : function(dim) {

                    vis.render.scales[dim].range(
                        vis.params.dims.ranges[dim]
                    )

                },

                domain : function(dim, variable) {

                    vis.render.scales[dim].domain(
                        vis.data.domains[variable]
                    )

                }

            }

        }


    },

    control : {

        init : function() {

            vis.utils.generates_refs();

            vis.utils.read_data(vis.params.refs.data);

        },

        set_dimensions : function() {

            // sizing, agrupar em uma coisa só, para ser chamada
            vis.utils.get_size();
            vis.utils.set_size();
            vis.utils.update_range("x");
            vis.utils.update_range("y");

            vis.render.scales.update.range("x");
            vis.render.scales.update.range("y");

        },


        begin : function(data) {

            console.log(data.columns);

            // saves data as a property to make it easier to access it elsewhere
            vis.data.raw = data;

            vis.utils.build_variables_domains();
            vis.utils.set_start_domain_zero("PL");

            vis.render.scales.update.domain("x", "PL");
            vis.render.scales.update.domain("y", "lucros");

            vis.control.set_dimensions();

            console.log(vis);

        }

    }

}

vis.control.init();