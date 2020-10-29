const vis_mapa = {

    config : {

        urls_dados : {

            lista_setores:   "./dados/lista-setores.csv",
            setores_estados: "./dados/mapa-setores.csv"

        }

        
    },

    data : {

        lista_setores   : null,
        setores_estados : null
    
    },

    fs : {

        start : function() {
            Promise.all(
            [
                d3.csv(vis_mapa.config.urls_dados["lista_setores"]),
                d3.csv(vis_mapa.config.urls_dados["setores_estados"])
            ])
            .then(function(files) {
          
              vis_mapa.data.lista_setores   = files[0];
              vis_mapa.data.setores_estados = files[1];
              
            vis_mapa.fs.init();

            })
        },

        init : function() {

            console.table(vis_mapa.data.lista_setores);
            console.table(vis_mapa.data.setores_estados);

            vis_mapa.fs.popula_lista(vis_mapa.data.lista_setores);
        
        },

        popula_lista : function(dados_lista) {

            let seletor = d3.select("#seletor-setores");

            seletor
              .selectAll("option")
              .data(dados_lista)
              .join("option")
              .attr("value", d => d.setor)
              .text(d => d.setor);

        }

    }

}

vis_mapa.fs.start();


