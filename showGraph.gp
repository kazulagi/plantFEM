set label 'ID:1 Name: Nagoya' at .47839411           ,.74481645
set label 'ID:2 Name: Osaka' at .85790679           ,.09860463
set label 'ID:3 Name: Kyoto' at .80835430           ,.56117793
set label 'ID:4 Name: Tokyo' at .93906496           ,.96741820
set label 'ID:5 Name: Ishioka' at .08291202           ,.60800644
set label 'ID:6 Name: Hokkaido' at .62482615           ,.05598560
set label 'ID:7 Name: Hawaii' at .21596884           ,.22552119
set label 'ID:8 Name: Heaven' at .11426936           ,.28047117
set arrow 1 from .47839411           ,.74481645            to .85790679           ,.09860463
set arrow 2 from .47839411           ,.74481645            to .80835430           ,.56117793
set arrow 3 from .47839411           ,.74481645            to .93906496           ,.96741820
set arrow 4 from .47839411           ,.74481645            to .08291202           ,.60800644
set arrow 5 from .47839411           ,.74481645            to .62482615           ,.05598560
set arrow 6 from .47839411           ,.74481645            to .21596884           ,.22552119
set arrow 7 from .85790679           ,.09860463            to .47839411           ,.74481645
set arrow 8 from .85790679           ,.09860463            to .80835430           ,.56117793
set arrow 9 from .62482615           ,.05598560            to .85790679           ,.09860463
set arrow 10 from .80835430           ,.56117793            to .47839411           ,.74481645
set arrow 11 from .85790679           ,.09860463            to .80835430           ,.56117793
set arrow 12 from .93906496           ,.96741820            to .47839411           ,.74481645
set arrow 13 from .08291202           ,.60800644            to .47839411           ,.74481645
set arrow 14 from .62482615           ,.05598560            to .47839411           ,.74481645
set arrow 15 from .62482615           ,.05598560            to .85790679           ,.09860463
set arrow 16 from .21596884           ,.22552119            to .47839411           ,.74481645
plot './vertex.txt'
pause -1
