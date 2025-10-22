# data_QC

## Description de la base de données
La base de données data_QC contient 3 051 tweets collectés et annotés dans le cadre d’un mémoire de maîtrise sur le thème de la détection de la désinformation en contexte électoral québécois. Les tweets ont été récoltés sur Twitter/X entre le 28 août et le 3 octobre 2022, soit pendant la campagne électorale provinciale.

## Vérification de l'information
Chaque tweet a été vérifié manuellement à l’aide de :
 - Sites institutionnels (Gouvernement du Québec, Gouvernement du Canada, Élections Québec).
 - Médias établis (Le Devoir, La Presse, Radio-Canada/CBC, Journal de Montréal).
 - Médias locaux ou comptes officiels de partis/candidats, utilisés uniquement lorsque les trois premières catégories ne permettent pas de vérifier le contenu.
 - Bon sens pour les cas évidents (ex. « François Legault a été élu premier ministre du Québec en 2018 » → vrai).

## Définition des étiquettes
Chaque tweet est associé à une étiquette d’authenticité, répartie dans deux variables :

*trinary_label* <br>
La variable trinary_label propose une classification à trois niveaux permettant de maintenir la distinction entre la désinformation manifeste, l’information factuelle et les cas d’incertitude:
 - Vrai: la majorité des informations contenues dans le tweet est conforme à la réalité.
 - Faux: la majorité des informations contenues dans le tweet est inexacte ou trompeuse.
 - Non-vérifié: le tweet est considéré comme non vérifiable lorsqu’il ne contient pas suffisamment d’informations pour en évaluer l’authenticité, lorsqu’il s’agit d’une question ouverte ou encore lorsqu’aucune source n’a permis de confirmer ou d’infirmer son contenu.

*binary_label* <br>
La variable binary_label correspond à une version simplifiée de la précédente, réduisant la classification à deux catégories:
 - Vrai: la majorité des informations contenues dans le tweet est conforme à la réalité ou le tweet ne contient aucun élément de désinformation identifiable, malgré l'incertitude.
 - Faux: la majorité des informations contenues dans le tweet est inexacte ou trompeuse.

## Citation ##
Citation: Thibault, Camille. 2025. « Évaluation de grands modèles de langage en IA pour lutter contre la désinformation électorale au Québec ». Mémoire de M.Sc., Université de Montréal.

-------------------------------------------------------------------

## Dataset Description
Data_QC contains 2,220 tweets collected and annotated as part of a master’s thesis on disinformation detection in the context of the Quebec electoral campaign. The tweets were collected on Twitter/X between August 28 and October 3, 2022, during the provincial election campaign.

## Information Verification ##
Each tweet was manually verified using:
 - Institutional sites: Government of Quebec, Government of Canada, Elections Quebec
 - Established media outlets: Le Devoir, La Presse, Radio-Canada/CBC, Journal de Montréal
 - Local media or official accounts of parties/candidates, used only when the first three categories did not allow verification
 - Common sense for obvious cases (e.g., “François Legault was elected Premier of Quebec in 2018” → true)

## Label Definition ##
Each tweet is associated with an authenticity label, recorded in two variables:

*trinary_label* <br>
The trinary_label variable uses a three-level classification to distinguish between manifest misinformation, factual information, and cases of uncertainty:
 - True: the majority of information in the tweet is consistent with reality.
 - False: the majority of information in the tweet is inaccurate or misleading.
 - Unverified: the tweet does not contain sufficient factual elements to be evaluated, corresponds to an open question, or cannot be verified by any source.

*binary_label* <br>
The binary_label variable is a simplified, two-class version of the previous label:
 - True: the majority of information in the tweet is consistent with reality, or the tweet contains no identifiable misinformation despite uncertainty.
 - False: the majority of information in the tweet is inaccurate or misleading.

## Citation ##
Citation: Thibault, Camille. 2025. « Évaluation de grands modèles de langage en IA pour lutter contre la désinformation électorale au Québec ». M.Sc. thesis, Université de Montréal.
