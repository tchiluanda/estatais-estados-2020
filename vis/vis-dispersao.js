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
    
            }
    
        }      

    },

    data : {

        raw : null,

        processed : {},

        quant_variables : {

            // proprio de cada projeto
            list : ["lucros", "PL"],

            domains : {}
        
        },

        categ_variables : {

            // proprio de cada projeto
            // para este em particular, vou usar para definir as variaveis que pretendo usar
            // para permitir a filtragem dos dados. e também, no caso do "dep", para permitir 
            // estilizar os pontos usando css. para isso vou usar data-attributes.
            
            list : ["dep", "gov", "plr_rva", "cat_ROE", "setor", "Nome_estado"],
            
            domains : {}

        },

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

            vis.data.quant_variables.list.forEach(variable => {
                vis.data.quant_variables.domains[variable] = d3.extent(vis.data.raw, d => +d[variable]);
            })

            vis.data.categ_variables.list.forEach(variable => {
                vis.data.categ_variables.domains[variable] = utils.unique(vis.data.raw, variable);
            })

        },

        set_start_domain_zero : function(variable) {

            vis.data.quant_variables.domains[variable][0] = 0;

        },

        read_data : function(url) {

            d3.csv(url).then(
                data => vis.control.begin(data)
            );

        }

    },

    render : {

        scales : {

        }


    },

    control : {

        init : function() {

            vis.utils.generates_refs();
            vis.utils.get_size();
            vis.utils.set_size();

            vis.utils.read_data(vis.params.refs.data);

        },


        begin : function(data) {

            console.log(data.columns);
            console.table(data);

            // saves data as a property to make it easier to access it elsewhere
            vis.data.raw = data;

            vis.utils.build_variables_domains();
            vis.utils.set_start_domain_zero("PL");

        }

    }

}

vis.control.init();

console.log(vis);