# PaperMarioBattle

Essa é uma implementação do sistema de batalha do jogo Paper Mario and the Origami King na linguagem Elm

[Jogue!](https://paper-mario-battle-project.netlify.app/)

![Print](images/img.png)

No jogo, você desliza quadradinhos em volta de um círculo para alinhar os inimigos. Com eles alinhados, você pode usar suas armas para acertar o maior número deles possível.
Eu implementei só a parte de deslizar os quadradinhos. Os pontos azuis são quadrados vazios e os pontos vermelhos são os inimigos.

## Controles

```
k - Move a seleção no sentido horário
j - Move a seleção no sentido anti-horário

barra de espaço - Muda o modo de seleção de diamétrico para concêntrico e vice-versa
                  (Como se fossem linhas e colunas, só que em um círculo)

Shift + k - Desliza os quadrados para cima
Shift + j - Desliza os quadrados para baixo
u - Desfaz o último movimento (Cancela o movimento sendo feito se existir um)
```

No canto inferior direito tem um contador que mostra quantos movimentos já foram feitos e quantos movimentos são permitidos no total

Eu implementei somente um nível porque eu só queria mesmo implementar a mecânica do jogo e também porque provavelmente
seria algum tipo de infração de direitos autorias se eu colocasse vários.
