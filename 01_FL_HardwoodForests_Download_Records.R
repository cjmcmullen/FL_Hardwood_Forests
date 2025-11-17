## Download Record Data for FL_HardwoodForests
## Modified from Botany ENM workshop Cite: https://github.com/soltislab/BotanyENMWorkshops
### 9/4/2024
#


### Load packages 
library(dplyr) 
library(tidyr) 
library(plyr) 
library(spocc) 
library(ridigbio) 
library(tibble) 
#library(rbison)
#install.packages("devtools")
library(devtools)
#devtools::install_github("nataliepatten/gatoRs")
library(gatoRs)


### setwd 
setwd("/blue/soltis/share/FL_HardwoodForests/")



############################################################################
########################### 1. Make species lists ##########################
############################################################################

# Asaccharum <- list("Acer saccharum ssp. floridanum", "Acer barbatum forma floridanum", "Acer barbatum var. floridanum",
#                 "Acer floridanum", "Acer nigrum var. floridanum", "Acer saccharinum subsp. floridanum",
#                 "Acer saccharinum var. floridanum", "Acer saccharum var. floridanum", "Saccharodendron floridanum",
#                 "Acer barbatum forma commune", "Acer barbatum forma platylobum", "Acer barbatum var. longii",
#                 "Acer barbatum var. villipes", "Acer floridanum forma platylobum", "Acer floridanum forma villipes",
#                 "Acer floridanum var. longii", "Acer floridanum var. villipes", "Acer saccharum forma villipes",
#                 "Acer saccharum var. longii")
# 
# Aspinosa <- list("Aralia spinosa", "Angelica spinosa", "Aralia leroana", "Aralia spinosa forma subinermis", 
#               "Aralia spinosa var. inermis")
# 
# Aplatyneuron <- c("Asplenium platyneuron", "Acrostichum platyneuron", "Asplenium ebeneum", 
#                   "Asplenium ebeneum forma proliferum", "Asplenium ebeneum var. bacculum-rubrum", 
#                   "Asplenium ebeneum var. proliferum", "Asplenium platyneuron forma proliferum", 
#                   "Asplenium platyneuron var. bacculum-rubrum", "Asplenium platyneuron var. euroaustrinum", 
#                   "Athyrium platyneuron", "Chamaefilix platyneuron", "Trichomanes ebeneum", 
#                   "Asplenium ebeneum forma hortoniae", "Asplenium ebeneum var. hortoniae", 
#                   "Asplenium ebeneum var. incisum", "Asplenium platyneuron forma hortoniae", 
#                   "Asplenium platyneuron forma incisum", "Asplenium platyneuron var. hortoniae", 
#                   "Asplenium platyneuron var. incisum")
# 
# Acanadensis <- c("Aquilegia canadensis", "Aquilegia australis", "Aquilegia canadensis var. australis", 
#                  "Aquilegia elegans", "Aquilegia variegata", "Aquilegia canadensis forma albiflora", 
#                  "Aquilegia canadensis forma ecalcarata", "Aquilegia canadensis forma flaviflora", 
#                  "Aquilegia canadensis forma phippenii", "Aquilegia canadensis var. coccinea", 
#                  "Aquilegia canadensis var. eminens", "Aquilegia canadensis var. flaviflora", 
#                  "Aquilegia canadensis var. hybrida", "Aquilegia canadensis var. latiuscula", 
#                  "Aquilegia canadensis var. phippenii", "Aquilegia coccinea", "Aquilegia eminens", 
#                  "Aquilegia flaviflora", "Aquilegia latiuscula", "Aquilegia phoenicantha")
# 
# Cfloridus <- c("Calycanthus floridus", "Beureria ferax", "Beureria florida", "Butneria fertilis", 
#                "Butneria fertilis var. ferax", "Butneria fertilis var. glauca", "Butneria florida", 
#                "Calycanthus ferax", "Calycanthus fertilis", "Calycanthus fertilis var. ferax", 
#                "Calycanthus fertilis var. glaucus", "Calycanthus fertilis var. laevigatus", 
#                "Calycanthus fertilis var. oblongifolius", "Calycanthus floridus var. glaucus", 
#                "Calycanthus floridus var. laevigatus", "Calycanthus floridus var. oblongifolius", 
#                "Calycanthus glaucus", "Calycanthus glaucus var. oblongifolius", "Calycanthus laevigatus", 
#                "Calycanthus mollis", "Calycanthus oblongifolius", "Butneria fertilis forma nana", "Butneria mohrii", 
#                "Butneria nana", "Calycanthus asplenifolius", "Calycanthus bullatus", "Calycanthus fertilis forma nanus", 
#                "Calycanthus floridus var. inodorus", "Calycanthus floridus var. oblongus", 
#                "Calycanthus floridus var. ovatus", "Calycanthus glaucus forma oblongus", 
#                "Calycanthus glaucus var. longifolius", "Calycanthus inodorus", "Calycanthus mohrii", 
#                "Calycanthus nanus", "Calycanthus reticulatus", "Calycanthus sterilis", "Calycanthus tomentosus", 
#                "Calycanthus verrucosus", "Calycanthus verrucosus var. cuneatus", 
#                "Calycanthus verrucosus var. multiflorus", "Calycanthus verrucosus var. parviflorus")
# 
# Cdasycarpa <- c("Carex dasycarpa", "Edritria dasycarpa")
# 
# Ccaroliniana <- c("Carpinus caroliniana", "Carpinus americana", "Carpinus ostryoides")
# 
# Cglabra <- c("Carya glabra", "Carya ashei", "Carya austrina", "Carya glabra subsp. megacarpa", 
#              "Carya glabra var. megacarpa", "Carya glabra var. odorata", "Carya leiodermis", "Carya magnifloridana", 
#              "Carya megacarpa", "Carya microcarpa", "Carya ovalis", "Carya ovalis var. megacarpa", 
#              "Carya ovalis var. odorata", "Carya pecan", "Hicorius ashei", "Hicorius austrina", "Hicorius glabra", 
#              "Hicorius glabra var. megacarpa", "Hicorius glabra var. odorata", "Hicorius leiodermis",
#              "Hicorius microcarpa", "Hicorius odorata", "Hicorius ovalis", "Hicorius ovalis var. megacarpa",
#              "Hicorius ovalis var. odorata", "Juglans alba var. odorata", "Juglans glabra", "Juglans ovalis",
#              "Juglans pecan")
# 
# Claevigata <- c("Celtis laevigata", "Celtis laevigata var. smallii", "Celtis mississippiensis", "Celtis smallii",
#                 "Celtis laevigata var. texana", "Celtis texana")
# 
# Coccidentalis <- c("Celtis occidentalis", "Celtis crassifolia", "Celtis georgiana",
#                    "Celtis mississippiensis var. pumila", "Celtis obliqua", "Celtis occidentalis forma pumila",
#                    "Celtis occidentalis subsp. georgiana", "Celtis occidentalis subsp. tenuifolia",
#                    "Celtis occidentalis var. crassifolia", "Celtis occidentalis var. georgiana", 
#                    "Celtis occidentalis var. integrifolia", "Celtis occidentalis var. pumila", "Celtis pumila", 
#                    "Celtis pumila var. georgiana", "Celtis tenuifolia", "Celtis tenuifolia var. georgiana", 
#                    "Celtis canina", "Celtis occidentalis forma canina", "Celtis occidentalis var. canina", 
#                    "Celtis pumila var. deamii", "Celtis tenuifolia var. soperi")
# 
# Ccanadensis <- c("Cercis canadensis", "Cercis canadensis forma glabrifolia", "Cercis canadensis var. typica", 
#                  "Cercis canadensis var. pubescens", "Cercis dilatata", "Cercis ellipsoidea", "Cercis georgiana")
# 
# Claxum <- c("Chasmanthium laxum", "Chasmanthium gracile", "Holcus laxus", "Uniola gracilis", "Uniola laxa", 
#             "Uniola uniflora")
# 
# Cflorida <- c("Cornus florida", "Benthamia florida", "Benthamidia florida", "Cynoxylon floridum", 
#               "Benthamidia florida forma pendula", "Benthamidia florida forma pluribracteata", 
#               "Benthamidia florida forma xanthocarpa", "Benthamidia florida var. pendula", 
#               "Benthamidia florida var. rubra", "Cornus candidissima", "Cornus florida forma pendula", 
#               "Cornus florida forma pluribracteata", "Cornus florida forma rubra", 
#               "Cornus florida forma xanthocarpa", "Cornus florida var. pendula", "Cornus florida var. rosea", 
#               "Cornus florida var. rubra", "Cornus florida var. xanthocarpa", "Cynoxylon floridum var. pendulum", 
#               "Cynoxylon floridum var. rubrum")
# 
# Erepens <- c("Epigaea repens", "Epigaea repens forma plena", "Epigaea repens forma rosea", 
#              "Epigaea repens forma rubicunda", "Epigaea repens var. glabrifolia", "Epigaea repens var. rubicunda")
# 
# Eumbilicatum <- c("Erythronium umbilicatum")
# 
# Eamericanus <- c("Euonymus americanus", "Euonymus americanus var. angustifolius", 
#                  "Euonymus americanus var. obovatus", "Euonymus americanus var. sarmentosus", 
#                  "Euonymus angustifolius", "Euonymus muricatus", "Euonymus muricatus var. biflorus", 
#                  "Euonymus muricatus var. obliquatus", "Euonymus obovatus", "Euonymus sarmentosus", 
#                  "Euonymus sempervirens")
# 
# Fgrandifolia <- c("Fagus grandifolia", "Fagus americana", "Fagus ferruginea", "Fagus ferruginea var. caroliniana", 
#                   "Fagus grandifolia forma caroliniana", "Fagus grandifolia forma mollis", 
#                   "Fagus grandifolia var. caroliniana", "Fagus grandifolia var. typica", 
#                   "Fagus sylvatica var. americana", "Fagus sylvatica var. ferruginea", "Fagus alba", 
#                   "Fagus atropunicea", "Fagus grandifolia subsp. mexicana", "Fagus grandifolia var. mexicana", 
#                   "Fagus grandifolia var. pubescens", "Fagus heterophyla", "Fagus mexicana", "Fagus nigra", 
#                   "Fagus rotundifolia", "Fagus sylvatica var. atropunicea", "Fagus sylvestris", "Fagus virginiana")
# 
# Famericana <- c("Fraxinus americana", "Calycomelia americana", "Fraxinus americana subsp. typica", 
#                 "Fraxinus americana var. normalis", "Fraxinus glauca", "Leptalix glauca", "Calycomelia acuminata", 
#                 "Calycomelia alba", "Calycomelia biltmoreana", "Calycomelia epiptera", "Calycomelia juglandifolia", 
#                 "Fraxinus acuminata", "Fraxinus alba", "Fraxinus americana forma acuminata", 
#                 "Fraxinus americana forma ascidiata", "Fraxinus americana forma iodocarpa", 
#                 "Fraxinus americana subsp. biltmoreana", "Fraxinus americana subsp. novae-angliae", 
#                 "Fraxinus americana var. acuminata", "Fraxinus americana var. ascidiata", 
#                 "Fraxinus americana var. biltmoreana", "Fraxinus americana var. crassifolia", 
#                 "Fraxinus americana var. curtissii", "Fraxinus americana var. epiptera", 
#                 "Fraxinus americana var. iodocarpa", "Fraxinus americana var. juglandifolia", 
#                 "Fraxinus americana var. latifolia", "Fraxinus americana var. microcarpa", 
#                 "Fraxinus americana var. subcoriacea", "Fraxinus biltmoreana", "Fraxinus canadensis", 
#                 "Fraxinus caroliniensis", "Fraxinus catawbiensis", "Fraxinus curtissii", "Fraxinus epiptera", 
#                 "Fraxinus juglandifolia", "Fraxinus juglandifolia var. subserrata", "Fraxinus macrophylla", 
#                 "Fraxinus nigra var. juglandifolia", "Fraxinus novae-angliae", "Fraxinus viridis", "Leptalix acuminata", 
#                 "Leptalix alba", "Leptalix epiptera", "Leptalix juglandifolia", "Leptalix viridis")
# 
# Hdiptera <- c("Halesia diptera", "Carlomohria diptera", "Halesia diptera var. magniflora", "Mohria diptera", 
#               "Mohrodendron dipterum", "Halesia reticulata")
# 
# Harifolia <- c("Hexastylis arifolia", "Asarum arifolium var. callifolium", "Asarum arifolium var. ruthii", 
#                "Asarum callifolium", "Asarum ruthii", "Heterotropa arifolia", "Hexastylis arifolia", 
#                "Hexastylis arifolia var. callifolia", "Hexastylis arifolia var. ruthii", "Hexastylis callifolia", 
#                "Hexastylis ruthii")
# 
# Iopaca <- c("Ilex opaca", "Ageria opaca", "Ilex opaca forma subintegra", "Ilex opaca var. subintegra", 
#             "Ilex opaca var. xanthocarpa", "Ilex laxiflora", "Ilex opaca var. acuminata", "Ilex opaca var. floribunda", 
#             "Ilex opaca var. globosa", "Ilex opaca var. latifolia", "Ilex opaca var. laxiflora", 
#             "Ilex opaca var. macrodon", "Ilex opaca var. opaca", "Ilex quercifolia")
# 
# Lstyraciflua <- c("Liquidambar styraciflua", "Liquidambar barbata", "Liquidambar gummifera", "Liquidambar tuberculata", 
#                   "Liquidambar styraciflua forma pendula", "Liquidambar styraciflua forma rotundiloba", 
#                   "Liquidambar styraciflua forma suberosa")
# 
# Mgrandiflora <- c("Magnolia grandiflora", "Magnolia foetida", "Magnolia foetida forma margaretta", 
#                   "Magnolia grandiflora var. elliptica", "Magnolia grandiflora var. lanceolata", 
#                   "Magnolia grandiflora var. obovata", "Magnolia lacunosa", "Magnolia virginiana var. foetida", 
#                   "Magnolia ferruginea", "Magnolia foetida forma parvifolia", "Magnolia grandiflora var. ferruginea")
# 
# Mfloridana <- c("Matelea floridana", "Odontostephana floridana", "Vincetoxicum floridanum")
# 
# Malabamensis <- c("Matelea alabamensis", "Cyclodon alabamensis", "Vincetoxicum alabamense")
# 
# Mflavidula <- c("Matelea flavidula", "Gonolobus flavidulus", "Gonolobus hirsutus var. flavidulus", 
#                 "Odontostephana flavidula", "Vincetoxicum flavidulum", "Vincetoxicum hirsutum var. flavidulum", 
#                 "Gonolobus carolinensis")
# 
# Mrepens <- c("Mitchella repens", "Perdicesca repens", "Disperma repens", "Mitchella repens forma leucocarpa", 
#              "Mitchella repens var. alba")
# 
# Mreynoldsiae <- c("Monotropsis reynoldsiae", "Schweinitzia reynoldsiae")
# 
# Ohirtellus <- c("Oplismenus hirtellus", "Hippagrostis hirtella", "Milium undulatifolium", "Oplismenus parvifolius", 
#                 "Orthopogon hirtellus", "Orthopogon parvifolius", "Panicum hirtellum", "Panicum nuttallianum", 
#                 "Setaria hirtella", "Echinochloa cubensis", "Hekaterosachne elatior", "Hippagrostis loliacea", 
#                 "Oplismenus aemulus", "Oplismenus aemulus var. densiflorus", "Oplismenus aemulus var. flaccidus", 
#                 "Oplismenus aemulus var. lasiorhachis", "Oplismenus africanus", "Oplismenus aristulatus", 
#                 "Oplismenus borhidii", "Oplismenus brasiliensis", "Oplismenus chondrosioides", 
#                 "Oplismenus compositus var. imbecillis", "Oplismenus compositus var. loliaceus", "Oplismenus cubensis", 
#                 "Oplismenus depauperatus", "Oplismenus flaccidus", "Oplismenus hirtellus forma imbecillis", 
#                 "Oplismenus hirtellus forma lanceolatus", "Oplismenus hirtellus forma loliaceus", 
#                 "Oplismenus hirtellus subsp. acuminatus", "Oplismenus hirtellus subsp. fasciculatus", 
#                 "Oplismenus hirtellus subsp. imbecillis", "Oplismenus hirtellus subsp. loliaceus", 
#                 "Oplismenus hirtellus subsp. microphyllus", "Oplismenus hirtellus subsp. psilostachys", 
#                 "Oplismenus hirtellus subsp. tsushimensis", "Oplismenus hirtellus var. acuminatus", 
#                 "Oplismenus imbecillis", "Oplismenus imbecillis var. morrisonensis", "Oplismenus loliaceus", 
#                 "Oplismenus microphyllus", "Oplismenus minus", "Oplismenus psilostachys", 
#                 "Oplismenus setarius var. aemulus", "Oplismenus setarius var. imbecillis", 
#                 "Oplismenus tsushimensis", "Oplismenus undulatifolius var. imbecillis", 
#                 "Oplismenus undulatifolius var. microphyllus", "Oplismenus undulatifolius var. mollis", 
#                 "Oplismenus velutinus", "Orthopogon aemulus", "Orthopogon africanus", "Orthopogon cubensis", 
#                 "Orthopogon flaccidus", "Orthopogon imbecillis", "Orthopogon loliaceus", "Orthopogon velutinus", 
#                 "Panicum acuminatissimum", "Panicum aemulum", "Panicum africanum", "Panicum balfourii", 
#                 "Panicum barbifultum", "Panicum brevisetum", "Panicum cubense", "Panicum flaccidum", "Panicum imbecille", 
#                 "Panicum incanum", "Panicum loliaceum", "Panicum raddianum", "Panicum velutinum", "Setaria ocreata")
# 
# Ovirginiana <- c("Ostrya virginiana", "Carpinus triflora", "Carpinus virginiana", "Carpinus virginica", 
#                  "Ostrya carpinifolia var. virginica", "Ostrya italica subsp. virginiana", "Ostrya ostrya var. virginiana", 
#                  "Ostrya virginiana subsp. lasia", "Ostrya virginiana var. lasia", "Ostrya virginica", 
#                  "Ostrya virginica var. eglandulosa", "Zugilus virginica", "Carpinus ostrya var. americana", 
#                  "Ostrya americana", "Ostrya baileyi", "Ostrya mexicana", "Ostrya virginiana forma glandulosa", 
#                  "Ostrya virginiana subsp. guatemalensis", "Ostrya virginiana var. glandulosa", 
#                  "Ostrya virginiana var. guatemalensis", "Ostrya virginica var. glandulosa")
# 
# Pquinquefolia <- c("Parthenocissus quinquefolia", "Ampelopsis hederacea", "Ampelopsis hederacea var. murorum", 
#                    "Ampelopsis hirsuta", "Ampelopsis quinquefolia", "Cissus hederacea", "Cissus quinquefolia", 
#                    "Hedera quinquefolia", "Parthenocissus hirsuta", "Parthenocissus inserta", 
#                    "Parthenocissus quinquefolia forma hirsuta", "Parthenocissus quinquefolia var. hirsuta", 
#                    "Parthenocissus quinquefolia var. murorum", "Parthenocissus quinquefolia var. typica", 
#                    "Psedera hirsuta", "Psedera quinquefolia", "Psedera quinquefolia var. hirsuta", "Quinaria hederacea", 
#                    "Quinaria hirsuta", "Vitis hederacea", "Vitis hederacea var. hirsuta", "Vitis inserta", 
#                    "Vitis quinquefolia", "Vitis quinquefolia var. hirsuta", "Ampelopsis radicantissima var. saint-paulii", 
#                    "Ampelopsis saint-paulii", "Parthenocissus quinquefolia var. saint-paulii", 
#                    "Parthenocissus saint-paulii", "Psedera quinquefolia var. saint-paulii")
# 
# Pborbonia <- c("Persea borbonia", "Persea borbonia var. borbonia", "Borbonia borbonia", "Borbonia caroliniensis", 
#                "Borbonia humilis", "Borbonia littoralis", "Laurus borbonia", "Laurus caroliniensis", 
#                "Laurus caroliniensis var. glabra", "Persea caroliniensis", "Persea caroliniensis forma glabriuscula", 
#                "Persea caroliniensis var. glabriuscula", "Persea littoralis", "Tamala borbonia", 
#                "Tamala caroliniensis", "Tamala littoralis", "Laurus elongata", "Persea palustris var. laevifolia")
# 
# Pglabra <- c("Pinus glabra", "Pinus mitis var. paupera")
# 
# Ptaeda <- c("Pinus taeda", "Pinus taeda var. tenuifolia", "Pinus lutea")
# 
# Pcaroliniana <- c("Prunus caroliniana", "Cerasus caroliniana", "Laurocerasus caroliniana", "Padus caroliniana")
# 
# Pserotina <- c("Prunus serotina", "Cerasus serotina", "Padus alabamensis", "Padus cuthbertii", "Padus serotina", 
#                "Prunus alabamensis", "Prunus cuthbertii", "Prunus hirsuta", "Prunus serotina forma alabamensis", 
#                "Prunus serotina forma typica", "Prunus serotina var. alabamensis", "Cerasus serotina var. montana", 
#                "Padus australis", "Padus serotina var. neomontana", "Prunus australis", "Prunus capollin var. prophyllosa", 
#                "Prunus serotina subsp. hirsuta", "Prunus serotina var. montana", "Prunus serotina var. neomontana", 
#                "Prunus serotina var. smallii")
# 
# Qalba <- c("Quercus alba", "Quercus alba forma pinnatifida", "Quercus alba var. pinnatifida", 
#            "Quercus alba forma heterophylla", "Quercus alba forma latiloba", "Quercus alba forma longigemma", 
#            "Quercus alba forma microcarpa", "Quercus alba forma repanda", "Quercus alba forma ryderi", 
#            "Quercus alba forma sublyrata", "Quercus alba forma viridis", "Quercus alba var. latiloba", 
#            "Quercus alba var. microcarpa", "Quercus alba var. repanda", "Quercus nigrescens", "Quercus repanda", 
#            "Quercus retusa")
# 
# Qhemisphaerica <- c("Quercus hemisphaerica", "Dryopsila laurina", "Dryopsila maritima", 
#                     "Dryopsila verrucosa var. obliqua", "Quercus aquatica var. hybrida", "Quercus aquatica var. laurifolia", 
#                     "Quercus arenicola", "Quercus geminata forma maritima", "Quercus hemisphaerica", 
#                     "Quercus hemisphaerica var. maritima", "Quercus hybrida", "Quercus ilexoides", 
#                     "Quercus laurifolia forma dentata", "Quercus laurifolia subsp. maritima", 
#                     "Quercus laurifolia var. acuta", "Quercus laurifolia var. hybrida", "Quercus laurifolia var. maritima", 
#                     "Quercus laurifolia var. obtusa", "Quercus laurifolia var. rhombica", 
#                     "Quercus laurina", "Quercus maritima", "Quercus nigra forma hemisphaerica", "Quercus nitida", 
#                     "Quercus obtusa", "Quercus phellos var. laurifolia", "Quercus phellos var. maritima", 
#                     "Quercus rhombica", "Quercus suberoides", "Quercus uliginosa var. laurifolia", 
#                     "Quercus verrucosa var. obliqua", "Quercus virens var. maritima", "Quercus virginiana var. maritima", 
#                     "Quercus laurifolia forma arenaria", "Quercus laurifolia forma obovatifolia", 
#                     "Quercus obtusa var. obovatifolia", "Quercus rhombica var. obovatifolia", "Quercus rhombifolia")
# 
# Qmichauxii <- c("Quercus michauxii", "Quercus prinus var. michauxii", "Quercus versicolora")
# 
# Qvirginiana <- c("Quercus virginiana", "Dryopsila virens", "Quercus phellos var. obtusifolia", "Quercus virens", 
#                  "Quercus virginiana var. typica", "Quercus virginiana var. virescens", "Quercus andromeda", 
#                  "Quercus phellos var. sempervirens", "Quercus sempervirens", "Quercus virginiana var. macrophylla")
# 
# Slanuginosum <- c("Sideroxylon lanuginosum", "Bumelia lanuginosa", "Bumelia lanuginosa subsp. typica", "Bumelia rufa", 
#                   "Lyciodes lanuginosa", "Bumelia ferruginea", "Chrysophyllum ludovicianum")
# 
# Spumila <- c("Smilax pumila", "Smilax pubera", "Smilax puberula", "Smilax humilis")
# 
# Stinctoria <- c("Symplocos tinctoria", "Eugeniodes tinctoria", "Hopea tinctoria", "Protohopea tinctoria", 
#                 "Symplocos tinctoria var. ashei", "Symplocos tinctoria var. pygmaea")
# 
Tamericana <- c("Tilia americana subsp. caroliniana", "Tilia americana subsp. floridana", "Tilia americana var. floridana",
                "Tilia americana var. pubescens", "Tilia americana var. walteri", "Tilia ashei", "Tilia caroliniana", 
                "Tilia caroliniana subsp. floridana", "Tilia caroliniana var. floridana", "Tilia caroliniana var. vagans", 
                "Tilia crenoserrata", "Tilia crenoserrata var. acuminata", "Tilia floridana", 
                "Tilia floridana var. oblongifolia", "Tilia floridana var. porracea", "Tilia georgiana", "Tilia grata", 
                "Tilia leucocarpa var. oblongifolia", "Tilia porracea", "Tilia pubescens", 
                "Tilia pubescens forma heteromorpha", "Tilia pubescens var. aitonii", "Tilia alabamensis", 
                "Tilia americana var. laxiflora", "Tilia caroliniana var. rhoophila", "Tilia cocksii", 
                "Tilia floridana var. alabamensis", "Tilia floridana var. hypoleuca", "Tilia georgiana var. crinita", 
                "Tilia hypoleuca", "Tilia laxiflora", "Tilia leptophylla", "Tilia leucocarpa", 
                "Tilia leucocarpa forma attenuata", "Tilia leucocarpa var. brevipedunculata", "Tilia leucocarpa var. cocksii",
                "Tilia leucocarpa var. glaucescens", "Tilia littoralis", "Tilia littoralis var. discolor",
                "Tilia nigra var. laxiflora", "Tilia nuda", "Tilia nuda var. brevipedunculata",
                "Tilia nuda var. glaucescens", "Tilia phanera", "Tilia phanera var. scabrida",
                "Tilia pubescens forma glabrata", "Tilia pubescens forma gymnophylla", "Tilia pubescens forma ventenatii",
                "Tilia stenopetala", "Tilia texana", "Tilia texana var. grosseserrata", "Tilia truncata",
                "Tilia venulosa var. multinervis", "Tilia americana var. heterophylla", "Tilia americana subsp. heterophylla", "Tilia eburnea var. lasioclada",
                "Tilia heterophylla", "Tilia heterophylla var. amphibola", "Tilia heterophylla var. nivea",
                "Tilia lasioclada", "Tilia australis", "Tilia caroliniana var. lata", "Tilia eburnea",
                "Tilia floridana var. australis", "Tilia heterophylla var. michauxii",
                "Tilia heterophylla var. microdonta", "Tilia heterophylla var. tenera", "Tilia lata", "Tilia monticola",
                "Tilia opposita", "Tilia tenera", "Tilia venulosa")

# Ualata <- c("Ulmus alata", "Ulmus americana var. alata", "Ulmus longifolia")
# 
# Vsororia <- c("Viola sororia", "Viola affinis var. langloisii", "Viola alachuana", "Viola cucullata var. sororia", 
#               "Viola floridana", "Viola hirsutula", "Viola langloisii", "Viola missouriensis", 
#               "Viola palmata subsp. sororia", "Viola palmata var. sororia", "Viola papilionacea", 
#               "Viola sororia var. missouriensis", "Viola affinis forma albiflora", "Viola asarifolia", 
#               "Viola candidula", "Viola champlainensis", "Viola columbiana", "Viola communis", "Viola consobrina", 
#               "Viola consona", "Viola cordifolia", "Viola crenulata", "Viola cuspidata", "Viola domestica", 
#               "Viola domestica var. communis", "Viola dowelliana", "Viola filecetorum", "Viola fletcheri", 
#               "Viola illinoensis", "Viola latiuscula", "Viola lucidifolia", "Viola macounii", "Viola montivaga", 
#               "Viola nesiotica", "Viola nodosa", "Viola novae-angliae", "Viola obliqua var. crenulata", 
#               "Viola palmata var. asarifolia", "Viola papilionacea var. domestica", "Viola pratincola", 
#               "Viola priceana", "Viola rosacea", "Viola septentrionalis", "Viola septentrionalis forma alba", 
#               "Viola septentrionalis var. grisea", "Viola sororia forma pallida", "Viola sororia forma priceana", 
#               "Viola sororia var. grisea", "Viola sororia var. novae-angliae", "Viola subjuncta", "Viola subviscosa", 
#               "Viola thompsoniae", "Viola vagula", "Viola venustula", "Viola villosa var. cordifolia")
# 
# 
# 
# 
# ############################################################################
# ################## 2. Make species variable ######################
# ############################################################################
# 
# speciesList <- list(Asaccharum = Asaccharum, Aspinosa = Aspinosa, Acanadensis = Acanadensis,
#                     Aplatyneuron = Aplatyneuron, Cfloridus = Cfloridus, Cdasycarpa = Cdasycarpa,
#                     Ccaroliniana = Ccaroliniana, Cglabra = Cglabra,Claevigata = Claevigata, Coccidentalis = Coccidentalis,
#                     Ccanadensis = Ccanadensis, Claxum = Claxum, Cflorida = Cflorida, Erepens = Erepens, Eumbilicatum = Eumbilicatum,
#                     Eamericanus = Eamericanus, Fgrandifolia = Fgrandifolia, Famericana = Famericana,
#                     Hdiptera = Hdiptera, Harifolia = Harifolia, Iopaca = Iopaca, Lstyraciflua = Lstyraciflua,
#                     Mgrandiflora = Mgrandiflora, Mfloridana = Mfloridana, Malabamensis = Malabamensis,
#                     Mflavidula = Mflavidula, Mrepens = Mrepens, Mreynoldsiae = Mreynoldsiae, Ohirtellus = Ohirtellus,
#                     Ovirginiana = Ovirginiana, Pquinquefolia = Pquinquefolia, Pborbonia = Pborbonia, Pglabra = Pglabra,
#                     Ptaeda = Ptaeda, Pcaroliniana = Pcaroliniana, Pserotina = Pserotina, Qalba = Qalba, Qhemisphaerica = Qhemisphaerica,
#                     Qmichauxii = Qmichauxii, Qvirginiana = Qvirginiana, Slanuginosum = Slanuginosum, Spumila = Spumila,
#                     Stinctoria = Stinctoria, Tamericana = Tamericana, Ualata = Ualata, Vsororia = Vsororia)

#for submitting specific species:
speciesList <- list(Tamericana = Tamericana)

dir <- "/blue/soltis/share/FL_HardwoodForests/01_data/"

#dir.create(paste0(dir, "/", species))

############################################################################
###### 3. use gators_download to pull data from iDigBio, and GBIF #####
############################################################################

for (i in names(speciesList)){
    print(i)
    currentSpecies <- unlist(speciesList[[i]])
    
    # Try to download and handle cases where no records are found
    tryCatch({
      gators_download(synonyms.list = currentSpecies, 
                      write.file = TRUE, 
                      filename = paste0(dir, i, "_Nov_6_24.csv"), 
                      gbif.match = "fuzzy",
                      idigbio.filter = TRUE)
    }, warning = function(w) {
      # Handle warnings (e.g., no records found)
      message("Warning: ", conditionMessage(w), " for ", name)
    }, error = function(e) {
      # Handle errors
      message("Error: ", conditionMessage(e), " for ", name)
    })
    
}


