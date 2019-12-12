## Overview

A Oncase está contratando Data Scientists em diversos níveis de experiência para atuar nos projetos onde construimos, junto com nossos clientes, produtos analíticos.

Esse exercício de perguntas abertas consiste então em uma oportunidade de analisarmos o processo de decisão dos(as) candidatos(as) para atingir as repostas. 

## Critérios

Avaliaremos - os quesitos não estão necessariamente em ordem de prioridade:

* Sua capacidade na compreensão e modelagem dos dados e problbemas;
* Qualidade e proeficiência codificando na linguagem de sua escolha (R, Python);
* Qualidade das soluções encontradas para responder às perguntas;
* Comunicação na hora de questionar itens que não estejam claros;
* Comunicação visual eficaz (escrita e gráficos) no embasamento as respostas;
* Utilização de métodos adequados para cada problema;
* Análise de estudo e performance quando a resposta envolver a criação de modelos;

Conta ponto também:

* Aspectos de organização de código;
* Aspectos que demonstrem alguma maturidade em Data engineering/DataOps
* Repetibilidade do notebook

## Instruções

* Baixar o arquivo anexo receitas.json;
* Trabalhar nos dados e nas respostas em um notebook jupyter ou alguma outra plataforma de notebooks SaaS que consigamos ter acesso para avaliar os resultados;
* Submeter os resultados através de repositório git (Bitbucket, Github, etc) até a data de 1 semana após o recebimento desse material;
    * Repositórios privados devem ser compartilhados com bruno@oncase.com.br e marcello@oncase.com.br
* Dúvidas deverão ser tiradas via e-mail com bruno@oncase.com.br e marcello@oncase.com.br

## Exercício

> Arquivo: receitas.json - Sample ao final

1. A categorias pertencem as comidas mais calóricas?
2. Quais os top 10 ingredientes contidos nas receitas mais calóricas?
3. Se você tivesse que recomendar 3 receitas baseando-se nos dados, quais seriam?
4. Alguma característica presente nos dados determina a alta nota de uma receita?
5. Considerando-se as categorias das top 100 receitas em avaliação, quantas receitas há atualmente no site https://www.epicurious.com para cada categoria
6. [opcional] Construa um classificador para recomendar tags (categorias) para as receitas;

Sample:

```json
{
  "directions": [
    "Heat 2 tablespoons oil in heavy ...",
    "Place flour in shallow dish...",
    "Meanwhile, cook pasta in large pot of ...",
    "A dried herb ...."
  ],
  "sodium": 517,
  "date": "2004-08-20T04:00:00.000Z",
  "calories": 631,
  "categories": [
    "Milk/Cream",
    "Citrus",
    "Dairy",
    "Fish",
    "Garlic",
    "Pasta",
    "Sauté",
    "Quick & Easy",
    "Orange",
    "Snapper",
    "Summer",
    "Pan-Fry",
    "Bon Appétit"
  ],
  "desc": "Sharon Hooykaas of Los Alamitos, ...",
  "protein": 45,
  "fat": 24,
  "rating": 4.375,
  "title": "Snapper on Angel Hair with Citrus Cream ",
  "ingredients": [
    "4 tablespoons olive oil",
    "4 shallots, thinly sliced (about 1/2 cup)",
    "2 garlic cloves, chopped",
    "1 8-ounce bottle clam juice",
    "3/4 cup whipping cream",
    "1/2 cup white wine",
    "1/2 cup fresh orange juice",
    "2 tablespoons diced drained oil-packed sun-dried tomatoes",
    "1 tablespoon fresh lime juice",
    "1 teaspoon herbes de Provence* or salad herbs",
    "1 teaspoon Worcestershire sauce",
    "1/2 teaspoon grated orange peel",
    "1/2 teaspoon grated lime peel",
    "All purpose flour",
    "4 5- to 6-ounce red snapper fillets",
    "8 ounces angel hair pasta",
    "2 tablespoons thinly sliced fresh basil",
    "Additional minced orange peel"
  ]
}
```

