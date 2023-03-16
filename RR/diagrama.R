DiagrammeR(paste0("graph LR
A[<center><bold>",1,"</bold><br>Não Iniciado</center>] --> B(<center><bold>",pl_elab,"</bold><br>Incompletos</center>)
             B --> C{<center><bold>",pl_enviados,"</bold><br> Enviado ao MS</center>}
             C --> D[<center><bold>",pl_analise,"</bold><br> Em análise</center>]
             C --> E[<center><bold>",pl_diligencia,"</bold><br> Em diligência</center>]
             D  -->F{<center><bold>",pl_aprovado,"</bold><br> Aprovados</center>}
             E -->|reajuste|B
             D -->|reajuste|B
             B -->G{<center><bold>",pl_reenviados,"<br> Reenviados ao MS</center>}
             G --> H[<center><bold>",0,"<br> em reanálise</center>]
             G --> E
             H --> F
             H --> B
                  E -->F",collapse=""))

DiagrammeR(paste0("graph LR
             A[<center><bold>",1,"</bold><br>Não Iniciado</center>]
             A --> B(<center><bold>",pl_elab,"</bold><br>Incompletos</center>)
             B --> C[<center><bold>",pl_enviados+pl_reenviados,"</bold><br> Re/enviado ao MS</center>]
             C --> D[<center><bold>",pl_analise,"</bold><br> Em re/análise</center>]
             D --> E[<center><bold>",pl_diligencia,"</bold><br> Em diligência</center>]
             D  -->F{<center><bold>",pl_aprovado,"</bold><br> Aprovados</center>}
                  E -->F",collapse=""))
