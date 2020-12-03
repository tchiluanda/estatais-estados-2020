const vis = {

    params : {

        refs: {

            svg: "svg.vis-dispersao",
            cont: "div.vis-dispersao",
            data: "./dados/dados.csv"
    
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

        //

        variables : [""],

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

            // saves data as a property to make it easier to access it elsewhere
            vis.data.raw = data;

        }

    }

}

vis.control.init();