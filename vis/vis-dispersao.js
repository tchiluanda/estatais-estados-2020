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
        processed : {}

    },

    utils : {

        // "administrative tasks"

        generates_refs: function() {

            vis.sels.svg  = d3.select(vis.refs.svg);
            vis.sels.cont = d3.select(vis.refs.cont);

            vis.elems.svg  = document.querySelector(vis.refs.svg);
            vis.elems.cont = document.querySelector(vis.refs.cont);

        },

        get_size: function() {

            let win_w = window.getComputedStyle(vis.params.elems.cont).getPropertyValue("width").slice(0,-2); //window.innerWidth;
            let win_h = window.innerHeight;

            win_w = win_w > 680 ? 680 : win_w;

            let pos_vis_y = vis.elems.svg.getBoundingClientRect().y;

            vis.dims.h = win_h - pos_vis_y - vis.dims.margins.top - vis.dims.margins.bottom;
            // subtraio a margem para usar como margem
            vis.dims.w = +vis.sels.svg.style("width").slice(0, -2);

        },

        set_size: function() {

            vis.elems.svg.style.setProperty(
                "height", vis.dims.h + "px");

            //vis.elems.svg.style.setProperty("background-color", "coral");


        },

        read_data : function(url) {

            d3.csv(vis.refs.data).then(
                data => vis.control.begin(data)
            );

        }

    },

    control : {


        begin : function(data) {

            console.log(data.columns);

            // saves data as a property to make it easier to access it elsewhere
            vis.data.raw = data;

        }

    }

}