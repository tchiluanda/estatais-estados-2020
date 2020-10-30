const vis_mapa = {

    config : {

        urls_dados : {

            lista_setores:   "./dados/lista-setores.csv",
            setores_estados: "./dados/mapa-setores.csv"

        }

        
    },

    state : {

        setor : null

    },

    data : {

        lista_setores   : null,
        setores_estados : null
    
    },

    utils : {

        remove_acentos : function(str) {

            str = str.split(" ")[0];

            return str.replace(/[^a-zA-Z ]/g, "");

        }

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
            vis_mapa.fs.controla_seletor();
        
        },

        mostra_box_setor : function(setor) {

            console.log("Me chamaram para ativar o setor ", setor);

            d3.selectAll(".box-definicao").classed("ativo", false);

            d3.select("#box" + setor).classed("ativo", true);

        },

        controla_seletor : function() {

            let seletor = d3.select("#seletor-setores");

            seletor.on("change", function() {

                let opcao_escolhida = seletor.property("value");

                opcao_escolhida = vis_mapa.utils.remove_acentos(opcao_escolhida);
                vis_mapa.state.setor = opcao_escolhida;

                console.log("Opa, mudança! Vou ativar o setor ", opcao_escolhida);

                vis_mapa.fs.mostra_box_setor(opcao_escolhida);

            });
            
        },

        popula_lista : function(dados_lista) {

            let seletor = d3.select("#seletor-setores");

            seletor
              .selectAll("option")
              .data(dados_lista)
              .join("option")
              .attr("value", d => d.setor)
              .text(d => d.setor);

            let boxes = d3.select("div.boxes-definicao");

            boxes = boxes
              .selectAll("div.box-definicao")
              .data(dados_lista)
              .join("div")
              .classed("box-definicao", true)
              .attr("id", d => ("box" + vis_mapa.utils.remove_acentos(d.setor)));

            boxes
              .append("h2")
              .text(d => d.setor)
              .style("color", d => d.cores);

            boxes
              .append("p")
              .text(d => d.def)
              .style("background-color", d => d.cores)
              .style("color", (d,i) => i > 14 ? "rgb(59,55,52)" : "ghostwhite");

            

        }

    }

}

vis_mapa.fs.start();


