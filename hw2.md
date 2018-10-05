hw2
================
Kee-Young Shin
September 29, 2018

Problem 1
---------

### Create subdirectory

``` r
library(tidyverse)
```

    ## -- Attaching packages ------------------------------------------------------- tidyverse 1.2.1 --

    ## v ggplot2 3.0.0     v purrr   0.2.5
    ## v tibble  1.4.2     v dplyr   0.7.6
    ## v tidyr   0.8.1     v stringr 1.3.1
    ## v readr   1.1.1     v forcats 0.3.0

    ## -- Conflicts ---------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
dir.create("./hw2data")
```

    ## Warning in dir.create("./hw2data"): '.\hw2data' already exists

``` r
transit_data = read.csv("./hw2data/NYC_Transit_Subway_Entrance_And_Exit_Data.csv") %>%
  janitor::clean_names() %>% # clean name of data
  select(line:route11, entry, entrance_type, vending, ada) %>% # filter columns 
  mutate(entry = ifelse(entry == "YES", TRUE, FALSE)) %>% # convert character into logical
  unique()

transit_data
```

    ##                   line                            station_name
    ## 1             4 Avenue                                 25th St
    ## 3             4 Avenue                                 36th St
    ## 6             4 Avenue                                 45th St
    ## 10            4 Avenue                                 53rd St
    ## 14            4 Avenue                                 53rd St
    ## 15            4 Avenue                                 59th St
    ## 21            4 Avenue                                 77th St
    ## 23            4 Avenue                                 77th St
    ## 24            4 Avenue                                 86th St
    ## 27            4 Avenue                                 95th St
    ## 32            4 Avenue                                  9th St
    ## 33            4 Avenue                                  9th St
    ## 34            4 Avenue                Atlantic Av-Barclays Ctr
    ## 35            4 Avenue                            Bay Ridge Av
    ## 37            4 Avenue                            Bay Ridge Av
    ## 38            4 Avenue                               DeKalb Av
    ## 39            4 Avenue                               DeKalb Av
    ## 42            4 Avenue                               DeKalb Av
    ## 44            4 Avenue                              Pacific St
    ## 46            4 Avenue                             Prospect Av
    ## 49            4 Avenue                                Union St
    ## 53     42nd St Shuttle                           Grand Central
    ## 57     42nd St Shuttle                           Grand Central
    ## 58     42nd St Shuttle                           Grand Central
    ## 59     42nd St Shuttle                           Grand Central
    ## 60     42nd St Shuttle                            Times Square
    ## 61            6 Avenue                                 14th St
    ## 71            6 Avenue                                 23rd St
    ## 79            6 Avenue                                  2nd Av
    ## 83            6 Avenue                                 34th St
    ## 84            6 Avenue                                 34th St
    ## 91            6 Avenue                                 34th St
    ## 93            6 Avenue                                 42nd St
    ## 94            6 Avenue                                 42nd St
    ## 102           6 Avenue          47-50th Sts Rockefeller Center
    ## 108           6 Avenue          47-50th Sts Rockefeller Center
    ## 109           6 Avenue          47-50th Sts Rockefeller Center
    ## 113           6 Avenue          47-50th Sts Rockefeller Center
    ## 119           6 Avenue                                  4th Av
    ## 121           6 Avenue                                 57th St
    ## 129           6 Avenue                                  7th Av
    ## 137           6 Avenue                               Bergen St
    ## 141           6 Avenue                               Bergen St
    ## 143           6 Avenue                   Broadway-Lafayette St
    ## 146           6 Avenue                   Broadway-Lafayette St
    ## 148           6 Avenue                              Carroll St
    ## 153           6 Avenue                               Church Av
    ## 159           6 Avenue                             Delancey St
    ## 163           6 Avenue                           East Broadway
    ## 167           6 Avenue                   Fort Hamilton Parkway
    ## 170           6 Avenue                   Fort Hamilton Parkway
    ## 171           6 Avenue                                Grand St
    ## 174           6 Avenue                     Prospect Park-15 St
    ## 180           6 Avenue                            Smith-9th St
    ## 181           6 Avenue                                 York St
    ## 182        63rd Street                                 21st St
    ## 183        63rd Street                                 21st St
    ## 185        63rd Street                                 21st St
    ## 187        63rd Street                            Lexington Av
    ## 188        63rd Street                            Lexington Av
    ## 190        63rd Street                            Lexington Av
    ## 192        63rd Street                        Roosevelt Island
    ## 193           8 Avenue                                103rd St
    ## 194           8 Avenue                                116th St
    ## 198           8 Avenue                                125th St
    ## 204           8 Avenue                                135th St
    ## 209           8 Avenue                                135th St
    ## 210           8 Avenue                                145th St
    ## 216           8 Avenue                                 14th St
    ## 217           8 Avenue                                 14th St
    ## 221           8 Avenue                                 14th St
    ## 224           8 Avenue                                155th St
    ## 230           8 Avenue                 163rd St - Amsterdam Av
    ## 233           8 Avenue           168th St - Washington Heights
    ## 237           8 Avenue                                175th St
    ## 238           8 Avenue                                175th St
    ## 244           8 Avenue                                181st St
    ## 245           8 Avenue                                181st St
    ## 249           8 Avenue                                190th St
    ## 250           8 Avenue                                190th St
    ## 251           8 Avenue                                 23rd St
    ## 259           8 Avenue                                 23rd St
    ## 262           8 Avenue                                 34th St
    ## 263           8 Avenue                                 34th St
    ## 278           8 Avenue                                 42nd St
    ## 280           8 Avenue                                 42nd St
    ## 285           8 Avenue                                 42nd St
    ## 287           8 Avenue                                 50th St
    ## 290           8 Avenue                                 50th St
    ## 291           8 Avenue                                 50th St
    ## 292           8 Avenue                                 50th St
    ## 296           8 Avenue                                 50th St
    ## 297           8 Avenue                                 59th St
    ## 299           8 Avenue                                 59th St
    ## 300           8 Avenue                                 59th St
    ## 302           8 Avenue                                 59th St
    ## 307           8 Avenue                                 59th St
    ## 309           8 Avenue                                 72nd St
    ## 312           8 Avenue     81st St - Museum of Natural History
    ## 314           8 Avenue     81st St - Museum of Natural History
    ## 316           8 Avenue                                 86th St
    ## 320           8 Avenue                                 86th St
    ## 321           8 Avenue                                 96th St
    ## 324           8 Avenue                         Broadway-Nassau
    ## 326           8 Avenue                                Canal St
    ## 331           8 Avenue              Cathedral Parkway-110th St
    ## 334           8 Avenue                             Chambers St
    ## 342           8 Avenue                             Chambers St
    ## 343           8 Avenue                     Dyckman St-200th St
    ## 345           8 Avenue                     Dyckman St-200th St
    ## 346           8 Avenue                     Dyckman St-200th St
    ## 350           8 Avenue                                 High St
    ## 353           8 Avenue                       Inwood - 207th St
    ## 354           8 Avenue                       Inwood - 207th St
    ## 359           8 Avenue                               Spring St
    ## 361           8 Avenue                               Spring St
    ## 362           8 Avenue                             West 4th St
    ## 363           8 Avenue                             West 4th St
    ## 368           8 Avenue                      World Trade Center
    ## 376          Archer Av                        Jamaica-Van Wyck
    ## 377          Archer Av                        Jamaica-Van Wyck
    ## 379          Archer Av Parsons Blvd-Archer Av - Jamaica Center
    ## 380          Archer Av Parsons Blvd-Archer Av - Jamaica Center
    ## 383          Archer Av Parsons Blvd-Archer Av - Jamaica Center
    ## 389          Archer Av            Sutphin Blvd-Archer Av - JFK
    ## 393            Astoria                          30 Av-Grand Av
    ## 397            Astoria                     36 Av-Washington Av
    ## 400            Astoria                          39 Av-Beebe Av
    ## 402            Astoria                    Astoria Blvd-Hoyt Av
    ## 406            Astoria                                Broadway
    ## 409            Astoria                            Ditmars Blvd
    ## 413           Brighton                                  7th Av
    ## 415           Brighton                             Atlantic Av
    ## 416           Brighton                                    Av H
    ## 417           Brighton                                    Av H
    ## 418           Brighton                                    Av J
    ## 419           Brighton                                    Av J
    ## 420           Brighton                                    Av J
    ## 421           Brighton                                    Av M
    ## 422           Brighton                                    Av M
    ## 423           Brighton                                    Av U
    ## 424           Brighton                              Beverly Rd
    ## 425           Brighton                          Brighton Beach
    ## 426           Brighton                          Brighton Beach
    ## 433           Brighton                               Church Av
    ## 435           Brighton                            Cortelyou Rd
    ## 436           Brighton                           Kings Highway
    ## 439           Brighton                                 Neck Rd
    ## 440           Brighton                              Newkirk Av
    ## 441           Brighton                           Ocean Parkway
    ## 447           Brighton                             Parkside Av
    ## 448           Brighton                             Parkside Av
    ## 449           Brighton                           Prospect Park
    ## 450           Brighton                           Prospect Park
    ## 452           Brighton                          Sheepshead Bay
    ## 454           Brighton                            Stillwell Av
    ## 455           Brighton                             West 8th St
    ## 456           Brighton                             West 8th St
    ## 457           Broadway                                 23rd St
    ## 461           Broadway                                 23rd St
    ## 464           Broadway                                 23rd St
    ## 465           Broadway                                 28th St
    ## 469           Broadway                                 34th St
    ## 473           Broadway                                 49th St
    ## 475           Broadway                                 49th St
    ## 480           Broadway                                 57th St
    ## 488           Broadway                                  5th Av
    ## 492           Broadway                                  5th Av
    ## 495           Broadway                                  8th St
    ## 497           Broadway                                  8th St
    ## 502           Broadway                                  8th St
    ## 503           Broadway                           Canal St (UL)
    ## 510           Broadway                               City Hall
    ## 513           Broadway                            Cortlandt St
    ## 515           Broadway                            Cortlandt St
    ## 516           Broadway                            Cortlandt St
    ## 517           Broadway                                Court St
    ## 520           Broadway                             Lawrence St
    ## 527           Broadway                            Lexington Av
    ## 531           Broadway                               Prince St
    ## 535           Broadway                               Rector St
    ## 538           Broadway                               Rector St
    ## 543           Broadway                               Rector St
    ## 544           Broadway                    Times Square-42nd St
    ## 548           Broadway                            Union Square
    ## 551           Broadway                            Union Square
    ## 552           Broadway                            Whitehall St
    ## 554           Broadway                            Whitehall St
    ## 558   Broadway Jamaica                       104th St-102nd St
    ## 560   Broadway Jamaica                                111th St
    ## 562   Broadway Jamaica                                121st St
    ## 564   Broadway Jamaica                                121st St
    ## 565   Broadway Jamaica                                121st St
    ## 566   Broadway Jamaica                              Alabama Av
    ## 568   Broadway Jamaica                             Chauncey St
    ## 570   Broadway Jamaica                            Cleveland St
    ## 572   Broadway Jamaica                             Crescent St
    ## 574   Broadway Jamaica                           Cypress Hills
    ## 577   Broadway Jamaica                    Elderts Lane-75th St
    ## 579   Broadway Jamaica                             Flushing Av
    ## 581   Broadway Jamaica                             Flushing Av
    ## 583   Broadway Jamaica                  Forest Parkway-85th St
    ## 585   Broadway Jamaica                                Gates Av
    ## 587   Broadway Jamaica                               Halsey St
    ## 589   Broadway Jamaica                                Hewes St
    ## 591   Broadway Jamaica                            Kosciusko St
    ## 593   Broadway Jamaica                              Lorimer St
    ## 595   Broadway Jamaica                              Lorimer St
    ## 597   Broadway Jamaica                                Marcy Av
    ## 598   Broadway Jamaica                                Marcy Av
    ## 601   Broadway Jamaica                                Marcy Av
    ## 605   Broadway Jamaica                               Myrtle Av
    ## 607   Broadway Jamaica                              Norwood Av
    ## 609   Broadway Jamaica                           Van Siclen Av
    ## 611   Broadway Jamaica                          Woodhaven Blvd
    ## 614   Broadway-7th Ave                                103rd St
    ## 618   Broadway-7th Ave                                103rd St
    ## 619   Broadway-7th Ave            116th St-Columbia University
    ## 623   Broadway-7th Ave            116th St-Columbia University
    ## 624   Broadway-7th Ave                                125th St
    ## 627   Broadway-7th Ave                                125th St
    ## 628   Broadway-7th Ave                   137th St-City College
    ## 632   Broadway-7th Ave                                145th St
    ## 635   Broadway-7th Ave                                 14th St
    ## 638   Broadway-7th Ave                                 14th St
    ## 642   Broadway-7th Ave                                 14th St
    ## 643   Broadway-7th Ave                                157th St
    ## 647   Broadway-7th Ave                                168th St
    ## 649   Broadway-7th Ave                                168th St
    ## 650   Broadway-7th Ave                                181st St
    ## 651   Broadway-7th Ave                                181st St
    ## 652   Broadway-7th Ave                                 18th St
    ## 656   Broadway-7th Ave                                 18th St
    ## 658   Broadway-7th Ave                                191st St
    ## 659   Broadway-7th Ave                                191st St
    ## 660   Broadway-7th Ave                                207th St
    ## 662   Broadway-7th Ave                                207th St
    ## 663   Broadway-7th Ave                                207th St
    ## 664   Broadway-7th Ave                                215th St
    ## 666   Broadway-7th Ave                                215th St
    ## 667   Broadway-7th Ave                                215th St
    ## 668   Broadway-7th Ave                                231st St
    ## 670   Broadway-7th Ave                                231st St
    ## 672   Broadway-7th Ave                                238th St
    ## 673   Broadway-7th Ave                                238th St
    ## 675   Broadway-7th Ave                                 23rd St
    ## 679   Broadway-7th Ave                                 28th St
    ## 681   Broadway-7th Ave                                 28th St
    ## 685   Broadway-7th Ave                                 34th St
    ## 689   Broadway-7th Ave                                 34th St
    ## 690   Broadway-7th Ave                                 34th St
    ## 696   Broadway-7th Ave                                 50th St
    ## 700   Broadway-7th Ave                                 50th St
    ## 702   Broadway-7th Ave                 59th St-Columbus Circle
    ## 705   Broadway-7th Ave                 59th St-Columbus Circle
    ## 707   Broadway-7th Ave                  66th St-Lincoln Center
    ## 708   Broadway-7th Ave                  66th St-Lincoln Center
    ## 713   Broadway-7th Ave                                 72nd St
    ## 716   Broadway-7th Ave                                 72nd St
    ## 717   Broadway-7th Ave                                 79th St
    ## 721   Broadway-7th Ave                                 86th St
    ## 726   Broadway-7th Ave                                 96th St
    ## 727   Broadway-7th Ave                                 96th St
    ## 731   Broadway-7th Ave                                 96th St
    ## 732   Broadway-7th Ave                                Canal St
    ## 736   Broadway-7th Ave              Cathedral Parkway-110th St
    ## 739   Broadway-7th Ave                             Chambers St
    ## 744   Broadway-7th Ave                          Christopher St
    ## 749   Broadway-7th Ave                              Dyckman St
    ## 751   Broadway-7th Ave                             Franklin St
    ## 752   Broadway-7th Ave                             Franklin St
    ## 756   Broadway-7th Ave                              Houston St
    ## 764   Broadway-7th Ave                    Marble Hill-225th St
    ## 765   Broadway-7th Ave                    Marble Hill-225th St
    ## 766   Broadway-7th Ave                               Rector St
    ## 767   Broadway-7th Ave                               Rector St
    ## 771   Broadway-7th Ave                               Rector St
    ## 772   Broadway-7th Ave                             South Ferry
    ## 773   Broadway-7th Ave                             South Ferry
    ## 774   Broadway-7th Ave                             South Ferry
    ## 775   Broadway-7th Ave                             South Ferry
    ## 776   Broadway-7th Ave                             South Ferry
    ## 777   Broadway-7th Ave                            Times Square
    ## 778   Broadway-7th Ave                            Times Square
    ## 786   Broadway-7th Ave             Van Cortlandt Park-242nd St
    ## 790           Canarsie                                  1st Av
    ## 794           Canarsie                                  3rd Av
    ## 798           Canarsie                                  6th Av
    ## 800           Canarsie                                  8th Av
    ## 802           Canarsie                             Atlantic Av
    ## 805           Canarsie                              Bedford Av
    ## 809           Canarsie                             Bushwick Av
    ## 810           Canarsie             Canarsie - Rockaway Parkway
    ## 811           Canarsie             Canarsie - Rockaway Parkway
    ## 812           Canarsie             Canarsie - Rockaway Parkway
    ## 813           Canarsie                               DeKalb Av
    ## 819           Canarsie                               DeKalb Av
    ## 821           Canarsie                           East 105th St
    ## 823           Canarsie                               Graham Av
    ## 827           Canarsie                                Grand St
    ## 831           Canarsie                               Halsey St
    ## 835           Canarsie                               Halsey St
    ## 837           Canarsie                            Jefferson St
    ## 841           Canarsie                            Jefferson St
    ## 843           Canarsie                              Livonia Av
    ## 844           Canarsie                              Livonia Av
    ## 845           Canarsie                              Lorimer St
    ## 847           Canarsie                             Montrose Av
    ## 849           Canarsie                               Morgan Av
    ## 851           Canarsie                               Morgan Av
    ## 852           Canarsie                               Myrtle Av
    ## 855           Canarsie                             New Lots Av
    ## 856           Canarsie                               Sutter Av
    ## 857           Canarsie                            Union Square
    ## 859           Canarsie                               Wilson Av
    ## 860              Clark                            Borough Hall
    ## 861              Clark                            Borough Hall
    ## 864              Clark                                Clark St
    ## 866              Clark                               Fulton St
    ## 867              Clark                               Fulton St
    ## 870              Clark                              Park Place
    ## 871              Clark                                 Wall St
    ## 873              Clark                                 Wall St
    ## 877          Concourse                                155th St
    ## 878          Concourse                                167th St
    ## 883          Concourse                                167th St
    ## 884          Concourse                                170th St
    ## 889          Concourse                           174-175th Sts
    ## 890          Concourse                           174-175th Sts
    ## 893          Concourse                         182nd-183rd Sts
    ## 897          Concourse                       Bedford Park Blvd
    ## 902          Concourse                              Fordham Rd
    ## 908          Concourse                          Kingsbridge Rd
    ## 910          Concourse                          Kingsbridge Rd
    ## 913          Concourse                        Norwood-205th St
    ## 917          Concourse                              Tremont Av
    ## 922          Concourse                 Yankee Stadium-161st St
    ## 923          Concourse                 Yankee Stadium-161st St
    ## 927          Concourse                 Yankee Stadium-161st St
    ## 930       Coney Island                            Stillwell Av
    ## 931       Coney Island                             West 8th St
    ## 933          Crosstown                                 21st St
    ## 936          Crosstown                    Bedford-Nostrand Avs
    ## 942          Crosstown                                Broadway
    ## 946          Crosstown                              Classon Av
    ## 949          Crosstown                  Clinton-Washington Avs
    ## 950          Crosstown                  Clinton-Washington Avs
    ## 957          Crosstown                             Flushing Av
    ## 959          Crosstown                               Fulton St
    ## 961          Crosstown                           Greenpoint Av
    ## 966          Crosstown           Long Island City-Court Square
    ## 967          Crosstown           Long Island City-Court Square
    ## 968          Crosstown           Long Island City-Court Square
    ## 969          Crosstown                         Metropolitan Av
    ## 971          Crosstown                         Metropolitan Av
    ## 973          Crosstown                   Myrtle-Willoughby Avs
    ## 975          Crosstown                               Nassau Av
    ## 979          Crosstown                               Nassau Av
    ## 981             Culver                                 18th Av
    ## 985             Culver                                    Av I
    ## 987             Culver                                    Av I
    ## 989             Culver                                    Av N
    ## 993             Culver                                    Av P
    ## 995             Culver                                    Av U
    ## 997             Culver                                    Av U
    ## 999             Culver                                    Av X
    ## 1001            Culver                     Bay Parkway-22nd Av
    ## 1004            Culver                               Ditmas Av
    ## 1008            Culver                           Kings Highway
    ## 1012            Culver                   Neptune Av-Van Siclen
    ## 1014           Dyre Av                           Baychester Av
    ## 1015           Dyre Av                           Baychester Av
    ## 1016           Dyre Av                     Eastchester-Dyre Av
    ## 1017           Dyre Av                             Gun Hill Rd
    ## 1018           Dyre Av                             Gun Hill Rd
    ## 1019           Dyre Av                             Morris Park
    ## 1020           Dyre Av                          Pelham Parkway
    ## 1021   Eastern Parkway                Atlantic Av-Barclays Ctr
    ## 1022   Eastern Parkway                               Bergen St
    ## 1027   Eastern Parkway         Eastern Parkway-Brooklyn Museum
    ## 1029   Eastern Parkway                             Franklin Av
    ## 1033   Eastern Parkway                        Grand Army Plaza
    ## 1037   Eastern Parkway                                 Hoyt St
    ## 1040   Eastern Parkway                                 Hoyt St
    ## 1041   Eastern Parkway                                 Hoyt St
    ## 1042   Eastern Parkway                             Kingston Av
    ## 1044   Eastern Parkway                               Nevins St
    ## 1048   Eastern Parkway                             Nostrand Av
    ## 1050   Eastern Parkway                                Utica Av
    ## 1051   Eastern Parkway                                Utica Av
    ## 1057          Flushing                                103rd St
    ## 1061          Flushing                                111th St
    ## 1064          Flushing                    45 Rd-Court House Sq
    ## 1067          Flushing                                  5th Av
    ## 1068          Flushing                                  5th Av
    ## 1070          Flushing                 82nd St-Jackson Heights
    ## 1073          Flushing                        90th St Elmhurst
    ## 1076          Flushing                        Bliss St-46th St
    ## 1082          Flushing                        Broadway-74th St
    ## 1083          Flushing                         Fisk Av-69th St
    ## 1085          Flushing                        Flushing-Main St
    ## 1090          Flushing                        Flushing-Main St
    ## 1091          Flushing                        Flushing-Main St
    ## 1095          Flushing                   Grand Central-42nd St
    ## 1096          Flushing                           Hunters Point
    ## 1099          Flushing                           Junction Blvd
    ## 1100          Flushing                           Junction Blvd
    ## 1104          Flushing                      Lincoln Av-52nd St
    ## 1108          Flushing                       Lowery St-40th St
    ## 1112          Flushing                    Mets - Willets Point
    ## 1113          Flushing                    Mets - Willets Point
    ## 1116          Flushing                        Queensboro Plaza
    ## 1117          Flushing                        Queensboro Plaza
    ## 1118          Flushing                       Rawson St-33rd St
    ## 1124          Flushing                  Vernon Blvd-Jackson Av
    ## 1130          Flushing                  Vernon Blvd-Jackson Av
    ## 1132          Flushing                     Woodside Av-61st St
    ## 1133          Flushing                     Woodside Av-61st St
    ## 1135          Franklin                         Botanic Gardens
    ## 1136          Franklin                             Franklin Av
    ## 1137          Franklin                              Park Place
    ## 1139            Fulton         Broadway Junction-East New York
    ## 1140            Fulton                Clinton & Washington Avs
    ## 1143            Fulton                Clinton & Washington Avs
    ## 1144            Fulton                Clinton & Washington Avs
    ## 1145            Fulton                               Euclid Av
    ## 1146            Fulton                               Euclid Av
    ## 1150            Fulton                             Franklin Av
    ## 1151            Fulton                     Hoyt & Schermerhorn
    ## 1153            Fulton                   Jay St - Borough Hall
    ## 1155            Fulton                   Jay St - Borough Hall
    ## 1162            Fulton                   Jay St - Borough Hall
    ## 1166            Fulton                         Kingston-Throop
    ## 1170            Fulton                            Lafayette Av
    ## 1179            Fulton                              Liberty Av
    ## 1183            Fulton                             Nostrand Av
    ## 1187            Fulton                                Ralph Av
    ## 1190            Fulton                             Rockaway Av
    ## 1194            Fulton                             Rockaway Av
    ## 1196            Fulton                             Shepherd Av
    ## 1200            Fulton                                Utica Av
    ## 1204            Fulton                           Van Siclen Av
    ## 1208            Jerome                                138th St
    ## 1211            Jerome                149th St-Grand Concourse
    ## 1215            Jerome                                167th St
    ## 1219            Jerome                                170th St
    ## 1222            Jerome                                176th St
    ## 1224            Jerome                                183rd St
    ## 1227            Jerome        Bedford Park Blvd-Lehman College
    ## 1228            Jerome                             Burnside Av
    ## 1232            Jerome                              Fordham Rd
    ## 1236            Jerome                          Kingsbridge Rd
    ## 1239            Jerome                         Mosholu Parkway
    ## 1243            Jerome                              Mt Eden Av
    ## 1245            Jerome                                Woodlawn
    ## 1247            Jerome                 Yankee Stadium-161st St
    ## 1248            Jerome                 Yankee Stadium-161st St
    ## 1251            Jerome                 Yankee Stadium-161st St
    ## 1255             Lenox             110th St-Central Park North
    ## 1256             Lenox             110th St-Central Park North
    ## 1257             Lenox                                116th St
    ## 1261             Lenox                                125th St
    ## 1265             Lenox                                135th St
    ## 1269             Lenox                                145th St
    ## 1271             Lenox                                145th St
    ## 1273             Lenox                         Harlem-148th St
    ## 1274         Lexington                                103rd St
    ## 1276         Lexington                                110th St
    ## 1280         Lexington                                116th St
    ## 1284         Lexington                                125th St
    ## 1285         Lexington                                125th St
    ## 1289         Lexington                    14th St-Union Square
    ## 1290         Lexington                    14th St-Union Square
    ## 1293         Lexington                    14th St-Union Square
    ## 1295         Lexington                                 23rd St
    ## 1298         Lexington                                 23rd St
    ## 1304         Lexington                                 28th St
    ## 1308         Lexington                                 28th St
    ## 1311         Lexington                                 33rd St
    ## 1320         Lexington                                 51st St
    ## 1324         Lexington                                 51st St
    ## 1329         Lexington                                 59th St
    ## 1330         Lexington                                 59th St
    ## 1335         Lexington                                 59th St
    ## 1336         Lexington                  68th St-Hunter College
    ## 1337         Lexington                  68th St-Hunter College
    ## 1341         Lexington                                 77th St
    ## 1349         Lexington                                 86th St
    ## 1357         Lexington                                 96th St
    ## 1361         Lexington                             Astor Place
    ## 1363         Lexington                             Bleecker St
    ## 1370         Lexington                            Borough Hall
    ## 1375         Lexington                           Bowling Green
    ## 1376         Lexington                           Bowling Green
    ## 1379         Lexington                           Bowling Green
    ## 1380         Lexington               Brooklyn Bridge-City Hall
    ## 1381         Lexington               Brooklyn Bridge-City Hall
    ## 1382         Lexington               Brooklyn Bridge-City Hall
    ## 1383         Lexington               Brooklyn Bridge-City Hall
    ## 1384         Lexington               Brooklyn Bridge-City Hall
    ## 1385         Lexington               Brooklyn Bridge-City Hall
    ## 1386         Lexington               Brooklyn Bridge-City Hall
    ## 1387         Lexington               Brooklyn Bridge-City Hall
    ## 1389         Lexington                                Canal St
    ## 1391         Lexington                                Canal St
    ## 1393         Lexington                                Canal St
    ## 1394         Lexington                                Canal St
    ## 1396         Lexington                               Fulton St
    ## 1398         Lexington                               Fulton St
    ## 1399         Lexington                               Fulton St
    ## 1401         Lexington                   Grand Central-42nd St
    ## 1404         Lexington                   Grand Central-42nd St
    ## 1405         Lexington                   Grand Central-42nd St
    ## 1409         Lexington                               Spring St
    ## 1413         Lexington                                 Wall St
    ## 1420         Lexington                                 Wall St
    ## 1421           Liberty                      104th St-Oxford Av
    ## 1423           Liberty                   111th St-Greenwood Av
    ## 1427           Liberty                       80th St-Hudson St
    ## 1431           Liberty                         88th St-Boyd Av
    ## 1433           Liberty                                Grant Av
    ## 1434           Liberty                           Lefferts Blvd
    ## 1438           Liberty                           Rockaway Blvd
    ## 1442            Myrtle                              Central Av
    ## 1444            Myrtle                               Forest Av
    ## 1446            Myrtle                           Fresh Pond Rd
    ## 1448            Myrtle                        Knickerbocker Av
    ## 1450            Myrtle                         Metropolitan Av
    ## 1451            Myrtle                               Seneca Av
    ## 1453            Nassau                                  Bowery
    ## 1455            Nassau                                Broad St
    ## 1460            Nassau                                Broad St
    ## 1464            Nassau                                Canal St
    ## 1465            Nassau                             Chambers St
    ## 1466            Nassau                             Chambers St
    ## 1467            Nassau                                Essex St
    ## 1469            Nassau                               Fulton St
    ## 1472            Nassau                               Fulton St
    ## 1475          New Lots                               Junius St
    ## 1477          New Lots                             New Lots Av
    ## 1479          New Lots                         Pennsylvania Av
    ## 1481          New Lots                             Rockaway Av
    ## 1483          New Lots                             Saratoga Av
    ## 1486          New Lots                               Sutter Av
    ## 1489          New Lots                           Van Siclen Av
    ## 1491          Nostrand                              Beverly Rd
    ## 1492          Nostrand                              Beverly Rd
    ## 1493          Nostrand                               Church Av
    ## 1494          Nostrand                               Church Av
    ## 1500          Nostrand            Flatbush Av-Brooklyn College
    ## 1501          Nostrand            Flatbush Av-Brooklyn College
    ## 1508          Nostrand                              Newkirk Av
    ## 1510          Nostrand                              Newkirk Av
    ## 1511          Nostrand                            President St
    ## 1513          Nostrand                             Sterling St
    ## 1515          Nostrand                             Winthrop St
    ## 1516          Nostrand                             Winthrop St
    ## 1517            Pelham                        138th St-3rd Ave
    ## 1521            Pelham                        138th St-3rd Ave
    ## 1524            Pelham                                Brook Av
    ## 1528            Pelham                                Buhre Av
    ## 1531            Pelham                          Castle Hill Av
    ## 1533            Pelham                              Cypress Av
    ## 1537            Pelham              East 143rd St-St Mary's St
    ## 1541            Pelham                           East 149th St
    ## 1545            Pelham                                Elder Av
    ## 1547            Pelham                          Hunts Point Av
    ## 1549            Pelham                          Hunts Point Av
    ## 1550            Pelham                             Longwood Av
    ## 1554            Pelham                           Middletown Rd
    ## 1556            Pelham                Morrison Av-Soundview Av
    ## 1559            Pelham               Parkchester-East 177th St
    ## 1560            Pelham                         Pelham Bay Park
    ## 1561            Pelham                         Pelham Bay Park
    ## 1563            Pelham                         Pelham Bay Park
    ## 1565            Pelham                          St Lawrence Av
    ## 1567            Pelham      Westchester Square-East Tremont Av
    ## 1568            Pelham                             Whitlock Av
    ## 1570            Pelham                               Zerega Av
    ## 1572  Queens Boulevard                                169th St
    ## 1580  Queens Boulevard                          23rd St-Ely Av
    ## 1581  Queens Boulevard                          23rd St-Ely Av
    ## 1584  Queens Boulevard                                 36th St
    ## 1587  Queens Boulevard                                 36th St
    ## 1589  Queens Boulevard                                 46th St
    ## 1593  Queens Boulevard                          5th Av-53rd St
    ## 1594  Queens Boulevard                          5th Av-53rd St
    ## 1596  Queens Boulevard                          5th Av-53rd St
    ## 1598  Queens Boulevard                    63rd Drive-Rego Park
    ## 1600  Queens Boulevard                    63rd Drive-Rego Park
    ## 1605  Queens Boulevard                                 65th St
    ## 1607  Queens Boulevard                                 67th Av
    ## 1611  Queens Boulevard                                 75th Av
    ## 1614  Queens Boulevard                                  7th Av
    ## 1618  Queens Boulevard                 Briarwood-Van Wyck Blvd
    ## 1621  Queens Boulevard                             Elmhurst Av
    ## 1626  Queens Boulevard                    Forest Hills-71st Av
    ## 1628  Queens Boulevard                    Forest Hills-71st Av
    ## 1631  Queens Boulevard                        Grand Av-Newtown
    ## 1636  Queens Boulevard           Jackson Heights-Roosevelt Ave
    ## 1638  Queens Boulevard           Jackson Heights-Roosevelt Ave
    ## 1640  Queens Boulevard           Jackson Heights-Roosevelt Ave
    ## 1643  Queens Boulevard                        Jamaica-179th St
    ## 1644  Queens Boulevard                        Jamaica-179th St
    ## 1659  Queens Boulevard              Kew Gardens-Union Turnpike
    ## 1660  Queens Boulevard              Kew Gardens-Union Turnpike
    ## 1667  Queens Boulevard                    Lexington Av-53rd St
    ## 1670  Queens Boulevard                    Lexington Av-53rd St
    ## 1674  Queens Boulevard                           Northern Blvd
    ## 1676  Queens Boulevard                            Parsons Blvd
    ## 1682  Queens Boulevard                            Queens Plaza
    ## 1689  Queens Boulevard                             Steinway St
    ## 1693  Queens Boulevard                            Sutphin Blvd
    ## 1698  Queens Boulevard                          Woodhaven Blvd
    ## 1702          Rockaway                      Aqueduct Racetrack
    ## 1703          Rockaway               Aqueduct-North Conduit Av
    ## 1704          Rockaway               Aqueduct-North Conduit Av
    ## 1705          Rockaway                          Beach 105th St
    ## 1709          Rockaway                           Beach 25th St
    ## 1712          Rockaway                           Beach 36th St
    ## 1716          Rockaway                           Beach 44th St
    ## 1720          Rockaway                           Beach 60th St
    ## 1724          Rockaway                           Beach 67th St
    ## 1728          Rockaway                           Beach 90th St
    ## 1732          Rockaway                           Beach 98th St
    ## 1735          Rockaway                           Broad Channel
    ## 1736          Rockaway                    Far Rockaway-Mott Av
    ## 1737          Rockaway                            Howard Beach
    ## 1739          Rockaway                            Howard Beach
    ## 1740          Rockaway                            Howard Beach
    ## 1741          Rockaway               Rockaway Park-Beach 116th
    ## 1742         Sea Beach                                 18th Av
    ## 1743         Sea Beach                                 18th Av
    ## 1744         Sea Beach                                 20th Av
    ## 1745         Sea Beach                                 86th St
    ## 1746         Sea Beach                                  8th Av
    ## 1747         Sea Beach                                    Av U
    ## 1748         Sea Beach                                    Av U
    ## 1749         Sea Beach                     Bay Parkway-22nd Av
    ## 1751         Sea Beach                   Fort Hamilton Parkway
    ## 1753         Sea Beach                           Kings Highway
    ## 1754         Sea Beach                           Kings Highway
    ## 1755         Sea Beach                          New Utrecht Av
    ## 1757          West End                                 18th Av
    ## 1760          West End                                 20th Av
    ## 1764          West End                                 25th Av
    ## 1768          West End                                 50th St
    ## 1772          West End                                 55th St
    ## 1775          West End                                 62nd St
    ## 1778          West End                                 71st St
    ## 1782          West End                                 71st St
    ## 1784          West End                                 79th St
    ## 1786          West End                                 79th St
    ## 1788          West End                                  9th Av
    ## 1789          West End                             Bay 50th St
    ## 1793          West End                             Bay Parkway
    ## 1797          West End                   Fort Hamilton Parkway
    ## 1801 White Plains Road                         149th St-3rd Av
    ## 1802 White Plains Road                         149th St-3rd Av
    ## 1809 White Plains Road                                174th St
    ## 1813 White Plains Road                                219th St
    ## 1815 White Plains Road                                225th St
    ## 1818 White Plains Road                                233rd St
    ## 1820 White Plains Road                      238th St-Nereid Av
    ## 1822 White Plains Road                             Allerton Av
    ## 1824 White Plains Road                         Bronx Park East
    ## 1827 White Plains Road                                Burke Av
    ## 1829 White Plains Road                           East 180th St
    ## 1831 White Plains Road           East Tremont Av-West Farms Sq
    ## 1832 White Plains Road           East Tremont Av-West Farms Sq
    ## 1836 White Plains Road                              Freeman St
    ## 1840 White Plains Road                             Gun Hill Rd
    ## 1841 White Plains Road                             Gun Hill Rd
    ## 1842 White Plains Road                            Intervale Av
    ## 1843 White Plains Road                            Intervale Av
    ## 1845 White Plains Road                              Jackson Av
    ## 1849 White Plains Road                          Pelham Parkway
    ## 1850 White Plains Road                          Pelham Parkway
    ## 1854 White Plains Road                             Prospect Av
    ## 1859 White Plains Road                              Simpson St
    ## 1860 White Plains Road                              Simpson St
    ## 1864 White Plains Road                      Wakefield-241st St
    ## 1867          Flushing                      34 St Hudson Yards
    ## 1868          Flushing                      34 St Hudson Yards
    ##      station_latitude station_longitude route1 route2 route3 route4 route5
    ## 1            40.66040         -73.99809      R                            
    ## 3            40.65514         -74.00355      N      R                     
    ## 6            40.64894         -74.01001      R                            
    ## 10           40.64507         -74.01403      R                            
    ## 14           40.64507         -74.01403      R                            
    ## 15           40.64136         -74.01788      N      R                     
    ## 21           40.62974         -74.02551      R                            
    ## 23           40.62974         -74.02551      R                            
    ## 24           40.62269         -74.02840      R                            
    ## 27           40.61662         -74.03088      R                            
    ## 32           40.67085         -73.98830      F      G      R              
    ## 33           40.67085         -73.98830      F      G      R              
    ## 34           40.68367         -73.97881      B      Q      D      N      R
    ## 35           40.63497         -74.02338      R                            
    ## 37           40.63497         -74.02338      R                            
    ## 38           40.69064         -73.98182      B      Q      R              
    ## 39           40.69064         -73.98182      B      Q      R              
    ## 42           40.69064         -73.98182      B      Q      R              
    ## 44           40.68367         -73.97881      B      Q      D      N      R
    ## 46           40.66541         -73.99287      R                            
    ## 49           40.67732         -73.98311      R                            
    ## 53           40.75277         -73.97919     GS      4      5      6      7
    ## 57           40.75277         -73.97919     GS      4      5      6      7
    ## 58           40.75277         -73.97919     GS      4      5      6      7
    ## 59           40.75277         -73.97919     GS      4      5      6      7
    ## 60           40.75598         -73.98623      A      C      E      N      Q
    ## 61           40.73823         -73.99621      F      L      M      1      2
    ## 71           40.74288         -73.99282      F      M                     
    ## 79           40.72340         -73.98994      F                            
    ## 83           40.74972         -73.98782      B      D      F      M      N
    ## 84           40.74972         -73.98782      B      D      F      M      N
    ## 91           40.74972         -73.98782      B      D      F      M      N
    ## 93           40.75422         -73.98457      B      D      F      M      7
    ## 94           40.75422         -73.98457      B      D      F      M      7
    ## 102          40.75866         -73.98133      B      D      F      M       
    ## 108          40.75866         -73.98133      B      D      F      M       
    ## 109          40.75866         -73.98133      B      D      F      M      7
    ## 113          40.75866         -73.98133      B      D      F      M       
    ## 119          40.67027         -73.98978      F                            
    ## 121          40.76397         -73.97745      F                            
    ## 129          40.66627         -73.98031      F                            
    ## 137          40.68615         -73.99086      F      G                     
    ## 141          40.68615         -73.99086      F      G                     
    ## 143          40.72530         -73.99620      B      D      F      M      6
    ## 146          40.72530         -73.99620      B      D      F      M      6
    ## 148          40.68030         -73.99505      F      G                     
    ## 153          40.64404         -73.97968      F                            
    ## 159          40.71861         -73.98811      F      J      M      Z       
    ## 163          40.71372         -73.99017      F                            
    ## 167          40.65078         -73.97578      F                            
    ## 170          40.65078         -73.97578      F                            
    ## 171          40.71827         -73.99375      B      D                     
    ## 174          40.66036         -73.97949      F                            
    ## 180          40.67358         -73.99596      F      G      R              
    ## 181          40.69974         -73.98689      F                            
    ## 182          40.75420         -73.94284      F                            
    ## 183          40.75420         -73.94284      F                            
    ## 185          40.75420         -73.94284      F                            
    ## 187          40.76463         -73.96611      F                            
    ## 188          40.76463         -73.96611      F                            
    ## 190          40.76463         -73.96611      F                            
    ## 192          40.75914         -73.95326      F                            
    ## 193          40.79609         -73.96145      B      C                     
    ## 194          40.80508         -73.95488      B      C                     
    ## 198          40.81111         -73.95234      A      B      C      D       
    ## 204          40.81789         -73.94765      B      C                     
    ## 209          40.81789         -73.94765      B      C                     
    ## 210          40.82478         -73.94422      A      B      C      D       
    ## 216          40.74089         -74.00169      A      C      E      L       
    ## 217          40.74089         -74.00169      A      C      E      L       
    ## 221          40.74089         -74.00169      A      C      E      L       
    ## 224          40.83052         -73.94151      C                            
    ## 230          40.83601         -73.93989      C                            
    ## 233          40.84072         -73.93956      A      C                     
    ## 237          40.84739         -73.93970      A                            
    ## 238          40.84739         -73.93970      A                            
    ## 244          40.85169         -73.93797      A                            
    ## 245          40.85169         -73.93797      A                            
    ## 249          40.85902         -73.93418      A                            
    ## 250          40.85902         -73.93418      A                            
    ## 251          40.74591         -73.99804      C      E                     
    ## 259          40.74591         -73.99804      C      E                     
    ## 262          40.75229         -73.99339      A      C      E              
    ## 263          40.75229         -73.99339      A      C      E              
    ## 278          40.75731         -73.98973      A      C      E      N      Q
    ## 280          40.75731         -73.98973      A      C      E      N      Q
    ## 285          40.75731         -73.98973      A      C      E      N      Q
    ## 287          40.76246         -73.98598      C      E                     
    ## 290          40.76246         -73.98598      C      E                     
    ## 291          40.76246         -73.98598      C      E                     
    ## 292          40.76246         -73.98598      C      E                     
    ## 296          40.76246         -73.98598      C      E                     
    ## 297          40.76830         -73.98174      A      B      C      D      1
    ## 299          40.76830         -73.98174      A      B      C      D      1
    ## 300          40.76830         -73.98174      A      B      C      D      1
    ## 302          40.76830         -73.98174      A      B      C      D      1
    ## 307          40.76830         -73.98174      A      B      C      D      1
    ## 309          40.77559         -73.97641      B      C                     
    ## 312          40.78143         -73.97214      B      C                     
    ## 314          40.78143         -73.97214      B      C                     
    ## 316          40.78587         -73.96892      B      C                     
    ## 320          40.78587         -73.96892      B      C                     
    ## 321          40.79165         -73.96470      B      C                     
    ## 324          40.71020         -74.00769      A      C      J      Z      2
    ## 326          40.72082         -74.00523      A      C      E              
    ## 331          40.80060         -73.95816      B      C                     
    ## 334          40.71411         -74.00858      A      C      E      2      3
    ## 342          40.71411         -74.00858      A      C      E      2      3
    ## 343          40.86549         -73.92727      A                            
    ## 345          40.86549         -73.92727      A                            
    ## 346          40.86549         -73.92727      A                            
    ## 350          40.69934         -73.99053      A      C      J      Z      2
    ## 353          40.86807         -73.91990      A                            
    ## 354          40.86807         -73.91990      A                            
    ## 359          40.72623         -74.00374      C      E                     
    ## 361          40.72623         -74.00374      C      E                     
    ## 362          40.73234         -74.00050      A      B      C      D      E
    ## 363          40.73234         -74.00050      A      B      C      D      E
    ## 368          40.71258         -74.00978      A      C      E      2      3
    ## 376          40.70257         -73.81686      E                            
    ## 377          40.70257         -73.81686      E                            
    ## 379          40.70215         -73.80111      E      J      Z              
    ## 380          40.70215         -73.80111      E      J      Z              
    ## 383          40.70215         -73.80111      E      J      Z              
    ## 389          40.70049         -73.80797      E      J      Z              
    ## 393          40.76678         -73.92148      N      Q                     
    ## 397          40.75680         -73.92957      N      Q                     
    ## 400          40.75288         -73.93276      N      Q                     
    ## 402          40.77026         -73.91784      N      Q                     
    ## 406          40.76182         -73.92551      N      Q                     
    ## 409          40.77504         -73.91203      N      Q                     
    ## 413          40.67705         -73.97237      B      Q                     
    ## 415          40.68446         -73.97689      B      Q      D      N      R
    ## 416          40.62927         -73.96164      B      Q                     
    ## 417          40.62927         -73.96164      B      Q                     
    ## 418          40.62504         -73.96080      B      Q                     
    ## 419          40.62504         -73.96080      B      Q                     
    ## 420          40.62504         -73.96080      B      Q                     
    ## 421          40.61762         -73.95940      B      Q                     
    ## 422          40.61762         -73.95940      B      Q                     
    ## 423          40.59930         -73.95593      B      Q                     
    ## 424          40.64403         -73.96449      B      Q                     
    ## 425          40.57762         -73.96138      B      Q                     
    ## 426          40.57762         -73.96138      B      Q                     
    ## 433          40.65053         -73.96298      B      Q                     
    ## 435          40.64093         -73.96389      B      Q                     
    ## 436          40.60867         -73.95773      B      Q                     
    ## 439          40.59525         -73.95516      B      Q                     
    ## 440          40.63508         -73.96279      B      Q                     
    ## 441          40.57631         -73.96850      Q                            
    ## 447          40.65529         -73.96149      B      Q                     
    ## 448          40.65529         -73.96149      B      Q                     
    ## 449          40.66161         -73.96225      B      Q     FS              
    ## 450          40.66161         -73.96225      B      Q     FS              
    ## 452          40.58690         -73.95416      B      Q                     
    ## 454          40.57742         -73.98123      D      F      N      Q       
    ## 455          40.57613         -73.97594      F      Q                     
    ## 456          40.57613         -73.97594      F      Q                     
    ## 457          40.74130         -73.98934      N      R                     
    ## 461          40.74130         -73.98934      N      R                     
    ## 464          40.74130         -73.98934      N      R                     
    ## 465          40.74549         -73.98869      N      R                     
    ## 469          40.74957         -73.98795      B      D      F      M      N
    ## 473          40.75990         -73.98414      N      Q      R              
    ## 475          40.75990         -73.98414      N      Q      R              
    ## 480          40.76466         -73.98066      N      Q      R              
    ## 488          40.76481         -73.97335      N      Q      R              
    ## 492          40.76481         -73.97335      N      Q      R              
    ## 495          40.73033         -73.99263      N      R                     
    ## 497          40.73033         -73.99263      N      R                     
    ## 502          40.73033         -73.99263      N      R                     
    ## 503          40.71953         -74.00177      J      N      Q      R      Z
    ## 510          40.71328         -74.00698      R                            
    ## 513          40.71067         -74.01103      R                            
    ## 515          40.71067         -74.01103      R                            
    ## 516          40.71067         -74.01103      R                            
    ## 517          40.69410         -73.99178      R      2      3      4      5
    ## 520          40.69218         -73.98594      A      C      F      R       
    ## 527          40.76266         -73.96726      N      Q      R      4      5
    ## 531          40.72433         -73.99770      N      R                     
    ## 535          40.70722         -74.01334      R                            
    ## 538          40.70722         -74.01334      R                            
    ## 543          40.70722         -74.01334      R                            
    ## 544          40.75467         -73.98675      A      C      E      N      Q
    ## 548          40.73574         -73.99057      L      N      Q      R      4
    ## 551          40.73574         -73.99057      L      N      Q      R      4
    ## 552          40.70309         -74.01299      R      1                     
    ## 554          40.70309         -74.01299      R      1                     
    ## 558          40.69518         -73.84433      J      Z                     
    ## 560          40.69742         -73.83634      J                            
    ## 562          40.70049         -73.82829      J      Z                     
    ## 564          40.70049         -73.82829      J      Z                     
    ## 565          40.70049         -73.82829      J      Z                     
    ## 566          40.67699         -73.89865      J                            
    ## 568          40.68289         -73.91046      J      Z                     
    ## 570          40.67995         -73.88464      J                            
    ## 572          40.68319         -73.87378      J      Z                     
    ## 574          40.68994         -73.87255      J                            
    ## 577          40.69132         -73.86714      J      Z                     
    ## 579          40.70026         -73.94113      J      M                     
    ## 581          40.70026         -73.94113      J      M                     
    ## 583          40.69244         -73.86001      J                            
    ## 585          40.68963         -73.92227      J      Z                     
    ## 587          40.68637         -73.91656      J                            
    ## 589          40.70687         -73.95343      J      M                     
    ## 591          40.69334         -73.92881      J                            
    ## 593          40.70387         -73.94741      J      M                     
    ## 595          40.70387         -73.94741      J      M                     
    ## 597          40.70836         -73.95776      J      M      Z              
    ## 598          40.70836         -73.95776      J      M      Z              
    ## 601          40.70836         -73.95776      J      M      Z              
    ## 605          40.69721         -73.93566      J      M      Z              
    ## 607          40.68141         -73.88004      J      Z                     
    ## 609          40.67802         -73.89169      J      Z                     
    ## 611          40.69388         -73.85158      J      Z                     
    ## 614          40.79945         -73.96838      1                            
    ## 618          40.79945         -73.96838      1                            
    ## 619          40.80772         -73.96411      1                            
    ## 623          40.80772         -73.96411      1                            
    ## 624          40.81558         -73.95837      1                            
    ## 627          40.81558         -73.95837      1                            
    ## 628          40.82201         -73.95368      1                            
    ## 632          40.82655         -73.95036      1                            
    ## 635          40.73783         -74.00020      F      L      M      1      2
    ## 638          40.73783         -74.00020      F      L      M      1      2
    ## 642          40.73783         -74.00020      F      L      M      1      2
    ## 643          40.83404         -73.94489      1                            
    ## 647          40.84056         -73.94013      A      C      1              
    ## 649          40.84056         -73.94013      A      C      1              
    ## 650          40.84951         -73.93360      1                            
    ## 651          40.84951         -73.93360      1                            
    ## 652          40.74104         -73.99787      1                            
    ## 656          40.74104         -73.99787      1                            
    ## 658          40.85522         -73.92941      1                            
    ## 659          40.85522         -73.92941      1                            
    ## 660          40.86461         -73.91882      1                            
    ## 662          40.86461         -73.91882      1                            
    ## 663          40.86461         -73.91882      1                            
    ## 664          40.86944         -73.91528      1                            
    ## 666          40.86944         -73.91528      1                            
    ## 667          40.86944         -73.91528      1                            
    ## 668          40.87886         -73.90483      1                            
    ## 670          40.87886         -73.90483      1                            
    ## 672          40.88467         -73.90087      1                            
    ## 673          40.88467         -73.90087      1                            
    ## 675          40.74408         -73.99566      1                            
    ## 679          40.74721         -73.99336      1                            
    ## 681          40.74721         -73.99336      1                            
    ## 685          40.75037         -73.99106      1      2      3              
    ## 689          40.75037         -73.99106      1      2      3              
    ## 690          40.75037         -73.99106      1      2      3              
    ## 696          40.76173         -73.98385      1                            
    ## 700          40.76173         -73.98385      1                            
    ## 702          40.76825         -73.98193      A      B      C      D      1
    ## 705          40.76825         -73.98193      A      B      C      D      1
    ## 707          40.77344         -73.98221      1                            
    ## 708          40.77344         -73.98221      1                            
    ## 713          40.77845         -73.98197      1      2      3              
    ## 716          40.77845         -73.98197      1      2      3              
    ## 717          40.78393         -73.97992      1                            
    ## 721          40.78864         -73.97622      1                            
    ## 726          40.79392         -73.97232      1      2      3              
    ## 727          40.79392         -73.97232      1      2      3              
    ## 731          40.79392         -73.97232      1      2      3              
    ## 732          40.72285         -74.00628      1                            
    ## 736          40.80397         -73.96685      1                            
    ## 739          40.71548         -74.00927      1      2      3              
    ## 744          40.73342         -74.00291      1                            
    ## 749          40.86053         -73.92554      1                            
    ## 751          40.71932         -74.00689      1                            
    ## 752          40.71932         -74.00689      1                            
    ## 756          40.72825         -74.00537      1                            
    ## 764          40.87456         -73.90983      1                            
    ## 765          40.87456         -73.90983      1                            
    ## 766          40.70751         -74.01378      1                            
    ## 767          40.70751         -74.01378      1                            
    ## 771          40.70751         -74.01378      1                            
    ## 772          40.70207         -74.01366      R      1                     
    ## 773          40.70207         -74.01366      R      1                     
    ## 774          40.70207         -74.01366      R      1                     
    ## 775          40.70207         -74.01366      R      1                     
    ## 776          40.70207         -74.01366      R      1                     
    ## 777          40.75529         -73.98749      A      C      E      N      Q
    ## 778          40.75529         -73.98749      A      C      E      N      Q
    ## 786          40.88925         -73.89858      1                            
    ## 790          40.73095         -73.98163      L                            
    ## 794          40.73285         -73.98612      L                            
    ## 798          40.73734         -73.99679      F      L      M      1      2
    ## 800          40.73978         -74.00258      A      C      E      L       
    ## 802          40.67535         -73.90310      L                            
    ## 805          40.71730         -73.95687      L                            
    ## 809          40.68283         -73.90525      L                            
    ## 810          40.64665         -73.90185      L                            
    ## 811          40.64576         -73.90250      L                            
    ## 812          40.64596         -73.90174      L                            
    ## 813          40.70381         -73.91842      L                            
    ## 819          40.70381         -73.91842      L                            
    ## 821          40.65057         -73.89948      L                            
    ## 823          40.71457         -73.94405      L                            
    ## 827          40.71193         -73.94067      L                            
    ## 831          40.69560         -73.90408      L                            
    ## 835          40.69560         -73.90408      L                            
    ## 837          40.70661         -73.92291      L                            
    ## 841          40.70661         -73.92291      L                            
    ## 843          40.66404         -73.90057      L                            
    ## 844          40.66404         -73.90057      L                            
    ## 845          40.71406         -73.95028      G      L                     
    ## 847          40.70774         -73.93985      L                            
    ## 849          40.70615         -73.93315      L                            
    ## 851          40.70615         -73.93315      L                            
    ## 852          40.69981         -73.91159      L      M                     
    ## 855          40.65873         -73.89923      L                            
    ## 856          40.66937         -73.90197      L                            
    ## 857          40.73479         -73.99073      L      N      Q      R      4
    ## 859          40.68876         -73.90405      L                            
    ## 860          40.69322         -73.99000      R      2      3      4      5
    ## 861          40.69322         -73.99000      R      2      3      4      5
    ## 864          40.69747         -73.99309      2      3                     
    ## 866          40.70942         -74.00657      A      C      J      Z      2
    ## 867          40.70942         -74.00657      A      C      J      Z      2
    ## 870          40.71305         -74.00881      A      C      E      1      2
    ## 871          40.70682         -74.00910      2      3                     
    ## 873          40.70682         -74.00910      2      3                     
    ## 877          40.83013         -73.93821      B      D                     
    ## 878          40.83377         -73.91843      B      D                     
    ## 883          40.83377         -73.91843      B      D                     
    ## 884          40.83931         -73.91340      B      D                     
    ## 889          40.84590         -73.91014      B      D                     
    ## 890          40.84590         -73.91014      B      D                     
    ## 893          40.85609         -73.90074      B      D                     
    ## 897          40.87324         -73.88714      B      D                     
    ## 902          40.86130         -73.89775      B      D                     
    ## 908          40.86698         -73.89351      B      D                     
    ## 910          40.86698         -73.89351      B      D                     
    ## 913          40.87481         -73.87886      D                            
    ## 917          40.85041         -73.90523      B      D                     
    ## 922          40.82791         -73.92565      B      D      4              
    ## 923          40.82791         -73.92565      B      D      4              
    ## 927          40.82791         -73.92565      B      D      4              
    ## 930          40.57742         -73.98123      D      F      N      Q       
    ## 931          40.57613         -73.97594      F      Q                     
    ## 933          40.74406         -73.94972      G                            
    ## 936          40.68963         -73.95352      G                            
    ## 942          40.70609         -73.95031      G                            
    ## 946          40.68887         -73.96007      G                            
    ## 949          40.68809         -73.96684      G                            
    ## 950          40.68809         -73.96684      G                            
    ## 957          40.70038         -73.95023      G                            
    ## 959          40.68712         -73.97537      G                            
    ## 961          40.73135         -73.95445      G                            
    ## 966          40.74655         -73.94383      G                            
    ## 967          40.74655         -73.94383      G                            
    ## 968          40.74655         -73.94383      G                            
    ## 969          40.71279         -73.95142      G      L                     
    ## 971          40.71279         -73.95142      G      L                     
    ## 973          40.69457         -73.94905      G                            
    ## 975          40.72463         -73.95128      G                            
    ## 979          40.72463         -73.95128      G                            
    ## 981          40.62976         -73.97697      F                            
    ## 985          40.62532         -73.97613      F                            
    ## 987          40.62532         -73.97613      F                            
    ## 989          40.61514         -73.97420      F                            
    ## 993          40.60894         -73.97302      F                            
    ## 995          40.59606         -73.97336      F                            
    ## 997          40.59606         -73.97336      F                            
    ## 999          40.58962         -73.97425      F                            
    ## 1001         40.62077         -73.97526      F                            
    ## 1004         40.63612         -73.97817      F                            
    ## 1008         40.60322         -73.97236      F                            
    ## 1012         40.58101         -73.97457      F                            
    ## 1014         40.87866         -73.83859      5                            
    ## 1015         40.87866         -73.83859      5                            
    ## 1016         40.88830         -73.83083      5                            
    ## 1017         40.86953         -73.84638      5                            
    ## 1018         40.86953         -73.84638      5                            
    ## 1019         40.85436         -73.86050      5                            
    ## 1020         40.85898         -73.85536      5                            
    ## 1021         40.68436         -73.97767      B      D      N      Q      R
    ## 1022         40.68083         -73.97510      2      3                     
    ## 1027         40.67199         -73.96438      2      3                     
    ## 1029         40.67068         -73.95813     FS      2      3      4      5
    ## 1033         40.67524         -73.97105      2      3                     
    ## 1037         40.69055         -73.98507      2      3                     
    ## 1040         40.69055         -73.98507      2      3                     
    ## 1041         40.69055         -73.98507      2      3                     
    ## 1042         40.66940         -73.94216      3                            
    ## 1044         40.68825         -73.98049      2      3      4      5       
    ## 1048         40.66985         -73.95047      3                            
    ## 1050         40.66890         -73.93294      3      4                     
    ## 1051         40.66890         -73.93294      3      4                     
    ## 1057         40.74986         -73.86270      7                            
    ## 1061         40.75173         -73.85533      7                            
    ## 1064         40.74702         -73.94526      E      G      M      7       
    ## 1067         40.75382         -73.98196      B      D      F      M      7
    ## 1068         40.75382         -73.98196      B      D      F      M      7
    ## 1070         40.74766         -73.88370      7                            
    ## 1073         40.74841         -73.87661      7                            
    ## 1076         40.74313         -73.91844      7                            
    ## 1082         40.74685         -73.89139      E      F      M      R      7
    ## 1083         40.74632         -73.89640      7                            
    ## 1085         40.75960         -73.83003      7                            
    ## 1090         40.75960         -73.83003      7                            
    ## 1091         40.75960         -73.83003      7                            
    ## 1095         40.75143         -73.97604     GS      4      5      6      7
    ## 1096         40.74222         -73.94892      7                            
    ## 1099         40.74914         -73.86953      7                            
    ## 1100         40.74914         -73.86953      7                            
    ## 1104         40.74415         -73.91255      7                            
    ## 1108         40.74378         -73.92402      7                            
    ## 1112         40.75462         -73.84562      7                            
    ## 1113         40.75462         -73.84562      7                            
    ## 1116         40.75058         -73.94020      N      Q      7              
    ## 1117         40.75058         -73.94020      N      Q      7              
    ## 1118         40.74459         -73.93100      7                            
    ## 1124         40.74263         -73.95358      7                            
    ## 1130         40.74263         -73.95358      7                            
    ## 1132         40.74563         -73.90298      7                            
    ## 1133         40.74563         -73.90298      7                            
    ## 1135         40.67034         -73.95924     FS      2      3      4      5
    ## 1136         40.68060         -73.95583      A     FS                     
    ## 1137         40.67477         -73.95762     FS                            
    ## 1139         40.67833         -73.90532      A      C      J      L       
    ## 1140         40.68326         -73.96584      C                            
    ## 1143         40.68326         -73.96584      C                            
    ## 1144         40.68326         -73.96584      C                            
    ## 1145         40.67538         -73.87211      A      C                     
    ## 1146         40.67538         -73.87211      A      C                     
    ## 1150         40.68138         -73.95685      A      C     FS              
    ## 1151         40.68848         -73.98500      A      C      G              
    ## 1153         40.69234         -73.98734      A      C      F      R       
    ## 1155         40.69234         -73.98734      A      C      F      R       
    ## 1162         40.69234         -73.98734      A      C      F      R       
    ## 1166         40.67992         -73.94086      A      C                     
    ## 1170         40.68611         -73.97395      C                            
    ## 1179         40.67454         -73.89655      A      C                     
    ## 1183         40.68044         -73.95043      A      C                     
    ## 1187         40.67882         -73.92079      A      C                     
    ## 1190         40.67834         -73.91195      A      C                     
    ## 1194         40.67834         -73.91195      A      C                     
    ## 1196         40.67413         -73.88075      A      C                     
    ## 1200         40.67936         -73.93073      A      C                     
    ## 1204         40.67271         -73.89036      A      C                     
    ## 1208         40.81322         -73.92985      4      5                     
    ## 1211         40.81838         -73.92735      2      4      5              
    ## 1215         40.83554         -73.92140      4                            
    ## 1219         40.84007         -73.91779      4                            
    ## 1222         40.84848         -73.91179      4                            
    ## 1224         40.85841         -73.90388      4                            
    ## 1227         40.87341         -73.89006      4                            
    ## 1228         40.85345         -73.90768      4                            
    ## 1232         40.86280         -73.90103      4                            
    ## 1236         40.86776         -73.89717      4                            
    ## 1239         40.87975         -73.88465      4                            
    ## 1243         40.84443         -73.91469      4                            
    ## 1245         40.88604         -73.87875      4                            
    ## 1247         40.82799         -73.92583     B       D      4              
    ## 1248         40.82799         -73.92583     B       D      4              
    ## 1251         40.82799         -73.92583     B       D      4              
    ## 1255         40.79908         -73.95182      2      3                     
    ## 1256         40.79908         -73.95182      2      3                     
    ## 1257         40.80210         -73.94962      2      3                     
    ## 1261         40.80775         -73.94549      2      3                     
    ## 1265         40.81423         -73.94077      2      3                     
    ## 1269         40.82042         -73.93624      3                            
    ## 1271         40.82042         -73.93624      3                            
    ## 1273         40.82388         -73.93647      3                            
    ## 1274         40.79060         -73.94748      6                            
    ## 1276         40.79502         -73.94425      6                            
    ## 1280         40.79863         -73.94162      6                            
    ## 1284         40.80414         -73.93759      4      5      6              
    ## 1285         40.80414         -73.93759      4      5      6              
    ## 1289         40.73467         -73.98995      L      N      Q      R      4
    ## 1290         40.73467         -73.98995      L      N      Q      R      4
    ## 1293         40.73467         -73.98995      L      N      Q      R      4
    ## 1295         40.73986         -73.98660      6                            
    ## 1298         40.73986         -73.98660      6                            
    ## 1304         40.74307         -73.98426      6                            
    ## 1308         40.74307         -73.98426      6                            
    ## 1311         40.74608         -73.98208      6                            
    ## 1320         40.75711         -73.97192      E      M      6              
    ## 1324         40.75711         -73.97192      E      M      6              
    ## 1329         40.76253         -73.96797      N      Q      R      4      5
    ## 1330         40.76253         -73.96797      N      Q      R      4      5
    ## 1335         40.76253         -73.96797      N      Q      R      4      5
    ## 1336         40.76814         -73.96387      6                            
    ## 1337         40.76814         -73.96387      6                            
    ## 1341         40.77362         -73.95987      6                            
    ## 1349         40.77949         -73.95559      4      5      6              
    ## 1357         40.78567         -73.95107      6                            
    ## 1361         40.73005         -73.99107      6                            
    ## 1363         40.72592         -73.99466      B      D      F      M      6
    ## 1370         40.69240         -73.99015      R      2      3      4      5
    ## 1375         40.70482         -74.01407      4      5                     
    ## 1376         40.70482         -74.01407      4      5                     
    ## 1379         40.70482         -74.01407      4      5                     
    ## 1380         40.71287         -74.00481      J      Z      4      5      6
    ## 1381         40.71272         -74.00497      J      Z      4      5      6
    ## 1382         40.71233         -74.00439      J      Z      4      5      6
    ## 1383         40.71186         -74.00511      J      Z      4      5      6
    ## 1384         40.71182         -74.00506      J      Z      4      5      6
    ## 1385         40.71381         -74.00391      J      Z      4      5      6
    ## 1386         40.71389         -74.00365      J      Z      4      5      6
    ## 1387         40.71307         -74.00413      J      Z      4      5      6
    ## 1389         40.71880         -74.00019      J      N      Q      R      Z
    ## 1391         40.71880         -74.00019      J      N      Q      R      Z
    ## 1393         40.71880         -74.00019      J      N      Q      R      Z
    ## 1394         40.71880         -74.00019      J      N      Q      R      Z
    ## 1396         40.71037         -74.00951      A      C      J      Z      2
    ## 1398         40.71037         -74.00951      A      C      J      Z      2
    ## 1399         40.71037         -74.00951      A      C      J      Z      2
    ## 1401         40.75178         -73.97685     GS      4      5      6      7
    ## 1404         40.75178         -73.97685     GS      4      5      6      7
    ## 1405         40.75178         -73.97685     GS      4      5      6      7
    ## 1409         40.72230         -73.99714      6                            
    ## 1413         40.70756         -74.01186      4      5                     
    ## 1420         40.70756         -74.01186      4      5                     
    ## 1421         40.68171         -73.83768      A                            
    ## 1423         40.68433         -73.83216      A                            
    ## 1427         40.67937         -73.85899      A                            
    ## 1431         40.67984         -73.85147      A                            
    ## 1433         40.67704         -73.86505      A                            
    ## 1434         40.68595         -73.82580      A                            
    ## 1438         40.68043         -73.84385      A                            
    ## 1442         40.69786         -73.92740      M                            
    ## 1444         40.70442         -73.90308      M                            
    ## 1446         40.70619         -73.89588      M                            
    ## 1448         40.69866         -73.91971      M                            
    ## 1450         40.71140         -73.88960      M                            
    ## 1451         40.70276         -73.90774      M                            
    ## 1453         40.72028         -73.99392      J      Z                     
    ## 1455         40.70648         -74.01106      J      Z                     
    ## 1460         40.70648         -74.01106      J      Z                     
    ## 1464         40.71809         -73.99989      J      N      Q      R      Z
    ## 1465         40.71324         -74.00340      J      Z      4      5      6
    ## 1466         40.71419         -74.00320      J      Z      4      5      6
    ## 1467         40.71831         -73.98744      F      J      M      Z       
    ## 1469         40.71037         -74.00758      A      C      J      Z      2
    ## 1472         40.71037         -74.00758      A      C      J      Z      2
    ## 1475         40.66351         -73.90245      3                            
    ## 1477         40.66624         -73.88408      3                            
    ## 1479         40.66463         -73.89490      3                            
    ## 1481         40.66255         -73.90895      3                            
    ## 1483         40.66145         -73.91633      3                            
    ## 1486         40.66472         -73.92261      3                            
    ## 1489         40.66545         -73.88939      3                            
    ## 1491         40.64510         -73.94896      2      5                     
    ## 1492         40.64510         -73.94896      2      5                     
    ## 1493         40.65084         -73.94957      2      5                     
    ## 1494         40.65084         -73.94957      2      5                     
    ## 1500         40.63284         -73.94764      2      5                     
    ## 1501         40.63284         -73.94764      2      5                     
    ## 1508         40.63997         -73.94841      2      5                     
    ## 1510         40.63997         -73.94841      2      5                     
    ## 1511         40.66788         -73.95068      2      5                     
    ## 1513         40.66274         -73.95085      2      5                     
    ## 1515         40.65665         -73.95020      2      5                     
    ## 1516         40.65665         -73.95020      2      5                     
    ## 1517         40.81048         -73.92614      6                            
    ## 1521         40.81048         -73.92614      6                            
    ## 1524         40.80757         -73.91924      6                            
    ## 1528         40.84681         -73.83257      6                            
    ## 1531         40.83425         -73.85122      6                            
    ## 1533         40.80537         -73.91404      6                            
    ## 1537         40.80872         -73.90766      6                            
    ## 1541         40.81212         -73.90410      6                            
    ## 1545         40.82858         -73.87916      6                            
    ## 1547         40.82095         -73.89055      6                            
    ## 1549         40.82095         -73.89055      6                            
    ## 1550         40.81610         -73.89643      6                            
    ## 1554         40.84386         -73.83632      6                            
    ## 1556         40.82952         -73.87452      6                            
    ## 1559         40.83323         -73.86082      6                            
    ## 1560         40.85246         -73.82812      6                            
    ## 1561         40.85246         -73.82812      6                            
    ## 1563         40.85246         -73.82812      6                            
    ## 1565         40.83151         -73.86762      6                            
    ## 1567         40.83989         -73.84295      6                            
    ## 1568         40.82652         -73.88628      6                            
    ## 1570         40.83649         -73.84704      6                            
    ## 1572         40.71047         -73.79360      F                            
    ## 1580         40.74785         -73.94600      E      G      M      7       
    ## 1581         40.74785         -73.94600      E      G      M      7       
    ## 1584         40.75204         -73.92878      M      R                     
    ## 1587         40.75204         -73.92878      M      R                     
    ## 1589         40.75631         -73.91333      M      R                     
    ## 1593         40.76017         -73.97522      E      M                     
    ## 1594         40.76017         -73.97522      E      M                     
    ## 1596         40.76017         -73.97522      E      M                     
    ## 1598         40.72985         -73.86160      M      R                     
    ## 1600         40.72985         -73.86160      M      R                     
    ## 1605         40.74967         -73.89845      M      R                     
    ## 1607         40.72652         -73.85272      M      R                     
    ## 1611         40.71833         -73.83732      E      F                     
    ## 1614         40.76286         -73.98164      B      D      E              
    ## 1618         40.70918         -73.82057      F                            
    ## 1621         40.74245         -73.88202      M      R                     
    ## 1626         40.72169         -73.84452      E      F      M      R       
    ## 1628         40.72169         -73.84452      e      F      M      R       
    ## 1631         40.73701         -73.87722      M      R                     
    ## 1636         40.74664         -73.89134      E      F      M      R      7
    ## 1638         40.74664         -73.89134      E      F      M      R      7
    ## 1640         40.74664         -73.89134      E      F      M      R      7
    ## 1643         40.71265         -73.78382      F                            
    ## 1644         40.71265         -73.78382      F                            
    ## 1659         40.71444         -73.83101      E      F                     
    ## 1660         40.71444         -73.83101      E      F                     
    ## 1667         40.75755         -73.96905      E      M      6              
    ## 1670         40.75755         -73.96905      E      M      6              
    ## 1674         40.75288         -73.90601      M      R                     
    ## 1676         40.70756         -73.80333      F                            
    ## 1682         40.74897         -73.93724      E      M      R              
    ## 1689         40.75688         -73.92074      M      R                     
    ## 1693         40.70546         -73.81071      F                            
    ## 1698         40.73311         -73.86923      M      R                     
    ## 1702         40.67213         -73.83581      A                            
    ## 1703         40.66823         -73.83406      A                            
    ## 1704         40.66823         -73.83406      A                            
    ## 1705         40.58321         -73.82756      H                            
    ## 1709         40.60007         -73.76135      A                            
    ## 1712         40.59540         -73.76817      A                            
    ## 1716         40.59294         -73.77601      A                            
    ## 1720         40.59237         -73.78852      A                            
    ## 1724         40.59093         -73.79692      A                            
    ## 1728         40.58803         -73.81364      H                            
    ## 1732         40.58531         -73.82056      H                            
    ## 1735         40.60838         -73.81592      A      H                     
    ## 1736         40.60399         -73.75540      A                            
    ## 1737         40.66048         -73.83030      A                            
    ## 1739         40.66048         -73.83030      A                            
    ## 1740         40.66048         -73.83030      A                            
    ## 1741         40.58090         -73.83559      H                            
    ## 1742         40.62067         -73.99041      N                            
    ## 1743         40.62067         -73.99041      N                            
    ## 1744         40.61741         -73.98503      N                            
    ## 1745         40.59272         -73.97823      N                            
    ## 1746         40.63506         -74.01172      N                            
    ## 1747         40.59747         -73.97914      N                            
    ## 1748         40.59747         -73.97914      N                            
    ## 1749         40.61181         -73.98185      N                            
    ## 1751         40.63139         -74.00535      N                            
    ## 1753         40.60392         -73.98035      N                            
    ## 1754         40.60392         -73.98035      N                            
    ## 1755         40.62484         -73.99635      D      N                     
    ## 1757         40.60795         -74.00174      D                            
    ## 1760         40.60456         -73.99817      D                            
    ## 1764         40.59770         -73.98683      D                            
    ## 1768         40.63626         -73.99479      D                            
    ## 1772         40.63144         -73.99548      D                            
    ## 1775         40.62647         -73.99689      D      N                     
    ## 1778         40.61959         -73.99886      D                            
    ## 1782         40.61959         -73.99886      D                            
    ## 1784         40.61350         -74.00061      D                            
    ## 1786         40.61350         -74.00061      D                            
    ## 1788         40.64629         -73.99432      D                            
    ## 1789         40.58884         -73.98377      D                            
    ## 1793         40.60187         -73.99373      D                            
    ## 1797         40.64091         -73.99430      D                            
    ## 1801         40.81611         -73.91776      2      5                     
    ## 1802         40.81611         -73.91776      2      5                     
    ## 1809         40.83729         -73.88773      2      5                     
    ## 1813         40.88390         -73.86263      2      5                     
    ## 1815         40.88802         -73.86034      2      5                     
    ## 1818         40.89319         -73.85747      2      5                     
    ## 1820         40.89838         -73.85438      2      5                     
    ## 1822         40.86546         -73.86735      2      5                     
    ## 1824         40.84883         -73.86846      2      5                     
    ## 1827         40.87136         -73.86716      2      5                     
    ## 1829         40.84189         -73.87349      2      5                     
    ## 1831         40.84029         -73.88005      2      5                     
    ## 1832         40.84029         -73.88005      2      5                     
    ## 1836         40.82999         -73.89186      2      5                     
    ## 1840         40.87785         -73.86626      2      5                     
    ## 1841         40.87785         -73.86626      2      5                     
    ## 1842         40.82218         -73.89674      2      5                     
    ## 1843         40.82218         -73.89674      2      5                     
    ## 1845         40.81649         -73.90781      2      5                     
    ## 1849         40.85719         -73.86762      2      5                     
    ## 1850         40.85719         -73.86762      2      5                     
    ## 1854         40.81958         -73.90177      2      5                     
    ## 1859         40.82407         -73.89306      2      5                     
    ## 1860         40.82407         -73.89306      2      5                     
    ## 1864         40.90313         -73.85062      2      5                     
    ## 1867         40.75588         -74.00191      7                            
    ## 1868         40.75588         -74.00191      7                            
    ##      route6 route7 route8 route9 route10 route11 entry entrance_type
    ## 1                      NA     NA      NA      NA  TRUE         Stair
    ## 3                      NA     NA      NA      NA  TRUE         Stair
    ## 6                      NA     NA      NA      NA  TRUE         Stair
    ## 10                     NA     NA      NA      NA  TRUE         Stair
    ## 14                     NA     NA      NA      NA FALSE         Stair
    ## 15                     NA     NA      NA      NA  TRUE         Stair
    ## 21                     NA     NA      NA      NA  TRUE         Stair
    ## 23                     NA     NA      NA      NA FALSE         Stair
    ## 24                     NA     NA      NA      NA  TRUE         Stair
    ## 27                     NA     NA      NA      NA  TRUE         Stair
    ## 32                     NA     NA      NA      NA  TRUE         Stair
    ## 33                     NA     NA      NA      NA  TRUE         Stair
    ## 34        2      3      4      5      NA      NA  TRUE      Elevator
    ## 35                     NA     NA      NA      NA  TRUE         Stair
    ## 37                     NA     NA      NA      NA FALSE         Stair
    ## 38                     NA     NA      NA      NA  TRUE      Elevator
    ## 39                     NA     NA      NA      NA  TRUE         Stair
    ## 42                     NA     NA      NA      NA  TRUE      Easement
    ## 44        2      3      4      5      NA      NA  TRUE         Stair
    ## 46                     NA     NA      NA      NA  TRUE         Stair
    ## 49                     NA     NA      NA      NA  TRUE         Stair
    ## 53                     NA     NA      NA      NA  TRUE      Easement
    ## 57                     NA     NA      NA      NA  TRUE     Escalator
    ## 58                     NA     NA      NA      NA FALSE      Easement
    ## 59                     NA     NA      NA      NA FALSE         Stair
    ## 60        R     GS      1      2       3       7  TRUE         Stair
    ## 61        3            NA     NA      NA      NA  TRUE         Stair
    ## 71                     NA     NA      NA      NA  TRUE         Stair
    ## 79                     NA     NA      NA      NA  TRUE         Stair
    ## 83        Q      R     NA     NA      NA      NA  TRUE      Elevator
    ## 84        Q      R     NA     NA      NA      NA  TRUE         Stair
    ## 91        Q      R     NA     NA      NA      NA  TRUE      Easement
    ## 93                     NA     NA      NA      NA  TRUE      Easement
    ## 94                     NA     NA      NA      NA  TRUE         Stair
    ## 102                    NA     NA      NA      NA  TRUE      Easement
    ## 108                    NA     NA      NA      NA  TRUE      Elevator
    ## 109                    NA     NA      NA      NA  TRUE         Stair
    ## 113                    NA     NA      NA      NA  TRUE         Stair
    ## 119                    NA     NA      NA      NA  TRUE          Door
    ## 121                    NA     NA      NA      NA  TRUE         Stair
    ## 129                    NA     NA      NA      NA  TRUE         Stair
    ## 137                    NA     NA      NA      NA  TRUE         Stair
    ## 141                    NA     NA      NA      NA  TRUE         Stair
    ## 143                    NA     NA      NA      NA  TRUE         Stair
    ## 146                    NA     NA      NA      NA FALSE         Stair
    ## 148                    NA     NA      NA      NA  TRUE         Stair
    ## 153                    NA     NA      NA      NA  TRUE         Stair
    ## 159                    NA     NA      NA      NA  TRUE         Stair
    ## 163                    NA     NA      NA      NA  TRUE         Stair
    ## 167                    NA     NA      NA      NA  TRUE         Stair
    ## 170                    NA     NA      NA      NA  TRUE          Door
    ## 171                    NA     NA      NA      NA  TRUE         Stair
    ## 174                    NA     NA      NA      NA  TRUE         Stair
    ## 180                    NA     NA      NA      NA  TRUE          Door
    ## 181                    NA     NA      NA      NA  TRUE         Stair
    ## 182                    NA     NA      NA      NA  TRUE      Elevator
    ## 183                    NA     NA      NA      NA  TRUE     Escalator
    ## 185                    NA     NA      NA      NA  TRUE         Stair
    ## 187                    NA     NA      NA      NA  TRUE      Elevator
    ## 188                    NA     NA      NA      NA  TRUE     Escalator
    ## 190                    NA     NA      NA      NA  TRUE         Stair
    ## 192                    NA     NA      NA      NA  TRUE          Door
    ## 193                    NA     NA      NA      NA  TRUE         Stair
    ## 194                    NA     NA      NA      NA  TRUE         Stair
    ## 198                    NA     NA      NA      NA  TRUE         Stair
    ## 204                    NA     NA      NA      NA  TRUE         Stair
    ## 209                    NA     NA      NA      NA  TRUE         Stair
    ## 210                    NA     NA      NA      NA  TRUE         Stair
    ## 216                    NA     NA      NA      NA  TRUE      Easement
    ## 217                    NA     NA      NA      NA  TRUE         Stair
    ## 221                    NA     NA      NA      NA FALSE         Stair
    ## 224                    NA     NA      NA      NA  TRUE         Stair
    ## 230                    NA     NA      NA      NA  TRUE         Stair
    ## 233                    NA     NA      NA      NA  TRUE         Stair
    ## 237                    NA     NA      NA      NA  TRUE      Elevator
    ## 238                    NA     NA      NA      NA  TRUE         Stair
    ## 244                    NA     NA      NA      NA  TRUE          Door
    ## 245                    NA     NA      NA      NA  TRUE         Stair
    ## 249                    NA     NA      NA      NA  TRUE          Door
    ## 250                    NA     NA      NA      NA  TRUE         Stair
    ## 251                    NA     NA      NA      NA  TRUE         Stair
    ## 259                    NA     NA      NA      NA FALSE         Stair
    ## 262                    NA     NA      NA      NA  TRUE      Elevator
    ## 263                    NA     NA      NA      NA  TRUE         Stair
    ## 278       R      S      1      2       3       7  TRUE      Easement
    ## 280       R      S      1      2       3       7  TRUE         Stair
    ## 285       R      S      1      2       3       7 FALSE         Stair
    ## 287                    NA     NA      NA      NA  TRUE      Easement
    ## 290                    NA     NA      NA      NA  TRUE      Elevator
    ## 291                    NA     NA      NA      NA  TRUE     Escalator
    ## 292                    NA     NA      NA      NA  TRUE         Stair
    ## 296                    NA     NA      NA      NA FALSE         Stair
    ## 297                    NA     NA      NA      NA  TRUE      Easement
    ## 299                    NA     NA      NA      NA  TRUE      Elevator
    ## 300                    NA     NA      NA      NA  TRUE     Escalator
    ## 302                    NA     NA      NA      NA  TRUE         Stair
    ## 307                    NA     NA      NA      NA  TRUE         Stair
    ## 309                    NA     NA      NA      NA  TRUE         Stair
    ## 312                    NA     NA      NA      NA  TRUE         Stair
    ## 314                    NA     NA      NA      NA  TRUE          Door
    ## 316                    NA     NA      NA      NA  TRUE         Stair
    ## 320                    NA     NA      NA      NA FALSE         Stair
    ## 321                    NA     NA      NA      NA  TRUE         Stair
    ## 324       3      4      5     NA      NA      NA  TRUE      Easement
    ## 326                    NA     NA      NA      NA  TRUE         Stair
    ## 331                    NA     NA      NA      NA  TRUE         Stair
    ## 334                    NA     NA      NA      NA  TRUE         Stair
    ## 342                    NA     NA      NA      NA FALSE         Stair
    ## 343                    NA     NA      NA      NA  TRUE      Easement
    ## 345                    NA     NA      NA      NA  TRUE         Stair
    ## 346                    NA     NA      NA      NA FALSE         Stair
    ## 350       3      4      5     NA      NA      NA  TRUE         Stair
    ## 353                    NA     NA      NA      NA  TRUE      Elevator
    ## 354                    NA     NA      NA      NA  TRUE         Stair
    ## 359                    NA     NA      NA      NA  TRUE         Stair
    ## 361                    NA     NA      NA      NA  TRUE         Stair
    ## 362       F      M     NA     NA      NA      NA  TRUE      Elevator
    ## 363       F      M     NA     NA      NA      NA  TRUE         Stair
    ## 368                    NA     NA      NA      NA  TRUE         Stair
    ## 376                    NA     NA      NA      NA  TRUE      Elevator
    ## 377                    NA     NA      NA      NA  TRUE         Stair
    ## 379                    NA     NA      NA      NA  TRUE      Elevator
    ## 380                    NA     NA      NA      NA  TRUE     Escalator
    ## 383                    NA     NA      NA      NA  TRUE         Stair
    ## 389                    NA     NA      NA      NA  TRUE         Stair
    ## 393                    NA     NA      NA      NA  TRUE         Stair
    ## 397                    NA     NA      NA      NA  TRUE         Stair
    ## 400                    NA     NA      NA      NA  TRUE         Stair
    ## 402                    NA     NA      NA      NA  TRUE         Stair
    ## 406                    NA     NA      NA      NA  TRUE         Stair
    ## 409                    NA     NA      NA      NA  TRUE         Stair
    ## 413                    NA     NA      NA      NA  TRUE         Stair
    ## 415       2      3      4      5      NA      NA  TRUE      Easement
    ## 416                    NA     NA      NA      NA  TRUE          Door
    ## 417                    NA     NA      NA      NA FALSE         Stair
    ## 418                    NA     NA      NA      NA  TRUE          Door
    ## 419                    NA     NA      NA      NA  TRUE         Stair
    ## 420                    NA     NA      NA      NA FALSE         Stair
    ## 421                    NA     NA      NA      NA  TRUE          Door
    ## 422                    NA     NA      NA      NA FALSE          Door
    ## 423                    NA     NA      NA      NA  TRUE          Door
    ## 424                    NA     NA      NA      NA  TRUE          Door
    ## 425                    NA     NA      NA      NA  TRUE     Escalator
    ## 426                    NA     NA      NA      NA  TRUE         Stair
    ## 433                    NA     NA      NA      NA  TRUE          Door
    ## 435                    NA     NA      NA      NA  TRUE          Door
    ## 436                    NA     NA      NA      NA  TRUE          Door
    ## 439                    NA     NA      NA      NA  TRUE          Door
    ## 440                    NA     NA      NA      NA  TRUE          Door
    ## 441                    NA     NA      NA      NA  TRUE         Stair
    ## 447                    NA     NA      NA      NA  TRUE          Door
    ## 448                    NA     NA      NA      NA FALSE         Stair
    ## 449                    NA     NA      NA      NA  TRUE          Door
    ## 450                    NA     NA      NA      NA  TRUE         Stair
    ## 452                    NA     NA      NA      NA  TRUE          Door
    ## 454                    NA     NA      NA      NA  TRUE         Stair
    ## 455                    NA     NA      NA      NA  TRUE          Ramp
    ## 456                    NA     NA      NA      NA  TRUE         Stair
    ## 457                    NA     NA      NA      NA  TRUE         Stair
    ## 461                    NA     NA      NA      NA  TRUE         Stair
    ## 464                    NA     NA      NA      NA FALSE         Stair
    ## 465                    NA     NA      NA      NA  TRUE         Stair
    ## 469       Q      R     NA     NA      NA      NA  TRUE         Stair
    ## 473                    NA     NA      NA      NA  TRUE         Stair
    ## 475                    NA     NA      NA      NA  TRUE      Easement
    ## 480                    NA     NA      NA      NA  TRUE         Stair
    ## 488                    NA     NA      NA      NA  TRUE         Stair
    ## 492                    NA     NA      NA      NA  TRUE      Easement
    ## 495                    NA     NA      NA      NA  TRUE         Stair
    ## 497                    NA     NA      NA      NA  TRUE         Stair
    ## 502                    NA     NA      NA      NA FALSE         Stair
    ## 503       6            NA     NA      NA      NA  TRUE         Stair
    ## 510                    NA     NA      NA      NA  TRUE         Stair
    ## 513                    NA     NA      NA      NA FALSE         Stair
    ## 515                    NA     NA      NA      NA  TRUE      Easement
    ## 516                    NA     NA      NA      NA  TRUE         Stair
    ## 517                    NA     NA      NA      NA  TRUE         Stair
    ## 520                    NA     NA      NA      NA  TRUE         Stair
    ## 527       6            NA     NA      NA      NA  TRUE         Stair
    ## 531                    NA     NA      NA      NA  TRUE         Stair
    ## 535                    NA     NA      NA      NA  TRUE         Stair
    ## 538                    NA     NA      NA      NA  TRUE         Stair
    ## 543                    NA     NA      NA      NA FALSE         Stair
    ## 544       R      S      1      2       3       7  TRUE         Stair
    ## 548       5      6     NA     NA      NA      NA  TRUE         Stair
    ## 551       5      6     NA     NA      NA      NA FALSE         Stair
    ## 552                    NA     NA      NA      NA  TRUE         Stair
    ## 554                    NA     NA      NA      NA  TRUE         Stair
    ## 558                    NA     NA      NA      NA  TRUE         Stair
    ## 560                    NA     NA      NA      NA  TRUE         Stair
    ## 562                    NA     NA      NA      NA  TRUE         Stair
    ## 564                    NA     NA      NA      NA  TRUE         Stair
    ## 565                    NA     NA      NA      NA FALSE         Stair
    ## 566                    NA     NA      NA      NA  TRUE         Stair
    ## 568                    NA     NA      NA      NA  TRUE         Stair
    ## 570                    NA     NA      NA      NA  TRUE         Stair
    ## 572                    NA     NA      NA      NA  TRUE         Stair
    ## 574                    NA     NA      NA      NA  TRUE         Stair
    ## 577                    NA     NA      NA      NA  TRUE         Stair
    ## 579                    NA     NA      NA      NA  TRUE      Elevator
    ## 581                    NA     NA      NA      NA  TRUE         Stair
    ## 583                    NA     NA      NA      NA  TRUE         Stair
    ## 585                    NA     NA      NA      NA  TRUE         Stair
    ## 587                    NA     NA      NA      NA  TRUE         Stair
    ## 589                    NA     NA      NA      NA  TRUE         Stair
    ## 591                    NA     NA      NA      NA  TRUE         Stair
    ## 593                    NA     NA      NA      NA  TRUE         Stair
    ## 595                    NA     NA      NA      NA FALSE         Stair
    ## 597                    NA     NA      NA      NA  TRUE      Elevator
    ## 598                    NA     NA      NA      NA  TRUE         Stair
    ## 601                    NA     NA      NA      NA  TRUE         Stair
    ## 605                    NA     NA      NA      NA  TRUE         Stair
    ## 607                    NA     NA      NA      NA  TRUE         Stair
    ## 609                    NA     NA      NA      NA  TRUE         Stair
    ## 611                    NA     NA      NA      NA  TRUE         Stair
    ## 614                    NA     NA      NA      NA  TRUE         Stair
    ## 618                    NA     NA      NA      NA FALSE         Stair
    ## 619                    NA     NA      NA      NA  TRUE         Stair
    ## 623                    NA     NA      NA      NA FALSE         Stair
    ## 624                    NA     NA      NA      NA  TRUE     Escalator
    ## 627                    NA     NA      NA      NA  TRUE         Stair
    ## 628                    NA     NA      NA      NA  TRUE         Stair
    ## 632                    NA     NA      NA      NA  TRUE         Stair
    ## 635       3            NA     NA      NA      NA  TRUE         Stair
    ## 638       3            NA     NA      NA      NA  TRUE         Stair
    ## 642       3            NA     NA      NA      NA FALSE         Stair
    ## 643                    NA     NA      NA      NA  TRUE         Stair
    ## 647                    NA     NA      NA      NA  TRUE         Stair
    ## 649                    NA     NA      NA      NA FALSE         Stair
    ## 650                    NA     NA      NA      NA  TRUE      Easement
    ## 651                    NA     NA      NA      NA  TRUE         Stair
    ## 652                    NA     NA      NA      NA  TRUE         Stair
    ## 656                    NA     NA      NA      NA FALSE         Stair
    ## 658                    NA     NA      NA      NA  TRUE          Door
    ## 659                    NA     NA      NA      NA  TRUE          Ramp
    ## 660                    NA     NA      NA      NA  TRUE         Stair
    ## 662                    NA     NA      NA      NA  TRUE         Stair
    ## 663                    NA     NA      NA      NA FALSE         Stair
    ## 664                    NA     NA      NA      NA  TRUE         Stair
    ## 666                    NA     NA      NA      NA  TRUE         Stair
    ## 667                    NA     NA      NA      NA FALSE         Stair
    ## 668                    NA     NA      NA      NA  TRUE         Stair
    ## 670                    NA     NA      NA      NA  TRUE         Stair
    ## 672                    NA     NA      NA      NA  TRUE         Stair
    ## 673                    NA     NA      NA      NA FALSE         Stair
    ## 675                    NA     NA      NA      NA  TRUE         Stair
    ## 679                    NA     NA      NA      NA  TRUE         Stair
    ## 681                    NA     NA      NA      NA FALSE         Stair
    ## 685                    NA     NA      NA      NA  TRUE      Easement
    ## 689                    NA     NA      NA      NA  TRUE         Stair
    ## 690                    NA     NA      NA      NA  TRUE         Stair
    ## 696                    NA     NA      NA      NA  TRUE         Stair
    ## 700                    NA     NA      NA      NA  TRUE      Easement
    ## 702                    NA     NA      NA      NA  TRUE         Stair
    ## 705                    NA     NA      NA      NA FALSE         Stair
    ## 707                    NA     NA      NA      NA  TRUE      Elevator
    ## 708                    NA     NA      NA      NA  TRUE         Stair
    ## 713                    NA     NA      NA      NA  TRUE          Door
    ## 716                    NA     NA      NA      NA FALSE          Door
    ## 717                    NA     NA      NA      NA  TRUE         Stair
    ## 721                    NA     NA      NA      NA  TRUE         Stair
    ## 726                    NA     NA      NA      NA  TRUE          Door
    ## 727                    NA     NA      NA      NA  TRUE         Stair
    ## 731                    NA     NA      NA      NA FALSE          Door
    ## 732                    NA     NA      NA      NA  TRUE         Stair
    ## 736                    NA     NA      NA      NA  TRUE         Stair
    ## 739                    NA     NA      NA      NA  TRUE         Stair
    ## 744                    NA     NA      NA      NA  TRUE         Stair
    ## 749                    NA     NA      NA      NA  TRUE         Stair
    ## 751                    NA     NA      NA      NA  TRUE         Stair
    ## 752                    NA     NA      NA      NA FALSE         Stair
    ## 756                    NA     NA      NA      NA  TRUE         Stair
    ## 764                    NA     NA      NA      NA  TRUE         Stair
    ## 765                    NA     NA      NA      NA  TRUE         Stair
    ## 766                    NA     NA      NA      NA  TRUE         Stair
    ## 767                    NA     NA      NA      NA  TRUE         Stair
    ## 771                    NA     NA      NA      NA FALSE         Stair
    ## 772                    NA     NA      NA      NA  TRUE      Elevator
    ## 773                    NA     NA      NA      NA  TRUE      Elevator
    ## 774                    NA     NA      NA      NA  TRUE     Escalator
    ## 775                    NA     NA      NA      NA  TRUE     Escalator
    ## 776                    NA     NA      NA      NA FALSE         Stair
    ## 777       R      S      1      2       3       7  TRUE      Easement
    ## 778       R      S      1      2       3       7  TRUE         Stair
    ## 786                    NA     NA      NA      NA  TRUE         Stair
    ## 790                    NA     NA      NA      NA  TRUE         Stair
    ## 794                    NA     NA      NA      NA  TRUE         Stair
    ## 798       3            NA     NA      NA      NA  TRUE         Stair
    ## 800                    NA     NA      NA      NA  TRUE         Stair
    ## 802                    NA     NA      NA      NA  TRUE         Stair
    ## 805                    NA     NA      NA      NA  TRUE         Stair
    ## 809                    NA     NA      NA      NA  TRUE          Door
    ## 810                    NA     NA      NA      NA  TRUE          Door
    ## 811                    NA     NA      NA      NA  TRUE          Door
    ## 812                    NA     NA      NA      NA  TRUE       Walkway
    ## 813                    NA     NA      NA      NA  TRUE         Stair
    ## 819                    NA     NA      NA      NA FALSE         Stair
    ## 821                    NA     NA      NA      NA  TRUE         Stair
    ## 823                    NA     NA      NA      NA  TRUE         Stair
    ## 827                    NA     NA      NA      NA  TRUE         Stair
    ## 831                    NA     NA      NA      NA  TRUE         Stair
    ## 835                    NA     NA      NA      NA FALSE         Stair
    ## 837                    NA     NA      NA      NA  TRUE         Stair
    ## 841                    NA     NA      NA      NA FALSE         Stair
    ## 843                    NA     NA      NA      NA  TRUE          Door
    ## 844                    NA     NA      NA      NA  TRUE         Stair
    ## 845                    NA     NA      NA      NA  TRUE         Stair
    ## 847                    NA     NA      NA      NA  TRUE         Stair
    ## 849                    NA     NA      NA      NA  TRUE         Stair
    ## 851                    NA     NA      NA      NA FALSE          Door
    ## 852                    NA     NA      NA      NA  TRUE         Stair
    ## 855                    NA     NA      NA      NA  TRUE         Stair
    ## 856                    NA     NA      NA      NA  TRUE         Stair
    ## 857       5      6     NA     NA      NA      NA  TRUE         Stair
    ## 859                    NA     NA      NA      NA  TRUE         Stair
    ## 860                    NA     NA      NA      NA  TRUE      Elevator
    ## 861                    NA     NA      NA      NA  TRUE         Stair
    ## 864                    NA     NA      NA      NA  TRUE          Door
    ## 866       3      4      5     NA      NA      NA  TRUE         Stair
    ## 867       3      4      5     NA      NA      NA FALSE      Easement
    ## 870       3            NA     NA      NA      NA  TRUE         Stair
    ## 871                    NA     NA      NA      NA  TRUE         Stair
    ## 873                    NA     NA      NA      NA  TRUE      Easement
    ## 877                    NA     NA      NA      NA  TRUE         Stair
    ## 878                    NA     NA      NA      NA  TRUE         Stair
    ## 883                    NA     NA      NA      NA FALSE         Stair
    ## 884                    NA     NA      NA      NA  TRUE         Stair
    ## 889                    NA     NA      NA      NA  TRUE          Door
    ## 890                    NA     NA      NA      NA  TRUE         Stair
    ## 893                    NA     NA      NA      NA  TRUE         Stair
    ## 897                    NA     NA      NA      NA  TRUE         Stair
    ## 902                    NA     NA      NA      NA  TRUE         Stair
    ## 908                    NA     NA      NA      NA  TRUE         Stair
    ## 910                    NA     NA      NA      NA  TRUE          Door
    ## 913                    NA     NA      NA      NA  TRUE         Stair
    ## 917                    NA     NA      NA      NA  TRUE         Stair
    ## 922                    NA     NA      NA      NA  TRUE      Elevator
    ## 923                    NA     NA      NA      NA  TRUE         Stair
    ## 927                    NA     NA      NA      NA  TRUE          Door
    ## 930                    NA     NA      NA      NA  TRUE         Stair
    ## 931                    NA     NA      NA      NA  TRUE         Stair
    ## 933                    NA     NA      NA      NA  TRUE         Stair
    ## 936                    NA     NA      NA      NA  TRUE         Stair
    ## 942                    NA     NA      NA      NA  TRUE         Stair
    ## 946                    NA     NA      NA      NA  TRUE         Stair
    ## 949                    NA     NA      NA      NA  TRUE         Stair
    ## 950                    NA     NA      NA      NA FALSE         Stair
    ## 957                    NA     NA      NA      NA  TRUE         Stair
    ## 959                    NA     NA      NA      NA  TRUE         Stair
    ## 961                    NA     NA      NA      NA  TRUE         Stair
    ## 966                    NA     NA      NA      NA  TRUE         Stair
    ## 967                    NA     NA      NA      NA FALSE      Easement
    ## 968                    NA     NA      NA      NA FALSE         Stair
    ## 969                    NA     NA      NA      NA  TRUE         Stair
    ## 971                    NA     NA      NA      NA  TRUE         Stair
    ## 973                    NA     NA      NA      NA  TRUE         Stair
    ## 975                    NA     NA      NA      NA  TRUE         Stair
    ## 979                    NA     NA      NA      NA  TRUE         Stair
    ## 981                    NA     NA      NA      NA  TRUE         Stair
    ## 985                    NA     NA      NA      NA  TRUE         Stair
    ## 987                    NA     NA      NA      NA FALSE         Stair
    ## 989                    NA     NA      NA      NA  TRUE         Stair
    ## 993                    NA     NA      NA      NA  TRUE         Stair
    ## 995                    NA     NA      NA      NA  TRUE         Stair
    ## 997                    NA     NA      NA      NA FALSE         Stair
    ## 999                    NA     NA      NA      NA  TRUE         Stair
    ## 1001                   NA     NA      NA      NA  TRUE         Stair
    ## 1004                   NA     NA      NA      NA  TRUE         Stair
    ## 1008                   NA     NA      NA      NA  TRUE         Stair
    ## 1012                   NA     NA      NA      NA  TRUE         Stair
    ## 1014                   NA     NA      NA      NA FALSE          Door
    ## 1015                   NA     NA      NA      NA  TRUE          Door
    ## 1016                   NA     NA      NA      NA  TRUE          Door
    ## 1017                   NA     NA      NA      NA FALSE      Easement
    ## 1018                   NA     NA      NA      NA  TRUE          Door
    ## 1019                   NA     NA      NA      NA  TRUE          Door
    ## 1020                   NA     NA      NA      NA  TRUE          Door
    ## 1021      2      3      4      5      NA      NA  TRUE      Easement
    ## 1022                   NA     NA      NA      NA  TRUE         Stair
    ## 1027                   NA     NA      NA      NA  TRUE         Stair
    ## 1029                   NA     NA      NA      NA  TRUE         Stair
    ## 1033                   NA     NA      NA      NA  TRUE         Stair
    ## 1037                   NA     NA      NA      NA  TRUE         Stair
    ## 1040                   NA     NA      NA      NA  TRUE         Stair
    ## 1041                   NA     NA      NA      NA FALSE         Stair
    ## 1042                   NA     NA      NA      NA  TRUE         Stair
    ## 1044                   NA     NA      NA      NA  TRUE         Stair
    ## 1048                   NA     NA      NA      NA  TRUE         Stair
    ## 1050                   NA     NA      NA      NA  TRUE      Elevator
    ## 1051                   NA     NA      NA      NA  TRUE         Stair
    ## 1057                   NA     NA      NA      NA  TRUE         Stair
    ## 1061                   NA     NA      NA      NA  TRUE         Stair
    ## 1064                   NA     NA      NA      NA  TRUE         Stair
    ## 1067                   NA     NA      NA      NA  TRUE         Stair
    ## 1068                   NA     NA      NA      NA FALSE         Stair
    ## 1070                   NA     NA      NA      NA  TRUE         Stair
    ## 1073                   NA     NA      NA      NA  TRUE         Stair
    ## 1076                   NA     NA      NA      NA  TRUE         Stair
    ## 1082                   NA     NA      NA      NA  TRUE         Stair
    ## 1083                   NA     NA      NA      NA  TRUE         Stair
    ## 1085                   NA     NA      NA      NA  TRUE         Stair
    ## 1090                   NA     NA      NA      NA  TRUE      Elevator
    ## 1091                   NA     NA      NA      NA  TRUE     Escalator
    ## 1095                   NA     NA      NA      NA  TRUE      Easement
    ## 1096                   NA     NA      NA      NA  TRUE         Stair
    ## 1099                   NA     NA      NA      NA  TRUE      Elevator
    ## 1100                   NA     NA      NA      NA  TRUE         Stair
    ## 1104                   NA     NA      NA      NA  TRUE         Stair
    ## 1108                   NA     NA      NA      NA  TRUE         Stair
    ## 1112                   NA     NA      NA      NA  TRUE          Door
    ## 1113                   NA     NA      NA      NA  TRUE         Stair
    ## 1116                   NA     NA      NA      NA  TRUE      Easement
    ## 1117                   NA     NA      NA      NA  TRUE         Stair
    ## 1118                   NA     NA      NA      NA  TRUE         Stair
    ## 1124                   NA     NA      NA      NA  TRUE         Stair
    ## 1130                   NA     NA      NA      NA FALSE         Stair
    ## 1132                   NA     NA      NA      NA  TRUE     Escalator
    ## 1133                   NA     NA      NA      NA  TRUE         Stair
    ## 1135                   NA     NA      NA      NA  TRUE         Stair
    ## 1136                   NA     NA      NA      NA  TRUE          Door
    ## 1137                   NA     NA      NA      NA  TRUE         Stair
    ## 1139                   NA     NA      NA      NA  TRUE          Door
    ## 1140                   NA     NA      NA      NA  TRUE         Stair
    ## 1143                   NA     NA      NA      NA  TRUE         Stair
    ## 1144                   NA     NA      NA      NA FALSE         Stair
    ## 1145                   NA     NA      NA      NA  TRUE      Elevator
    ## 1146                   NA     NA      NA      NA  TRUE         Stair
    ## 1150                   NA     NA      NA      NA  TRUE         Stair
    ## 1151                   NA     NA      NA      NA  TRUE         Stair
    ## 1153                   NA     NA      NA      NA  TRUE      Easement
    ## 1155                   NA     NA      NA      NA  TRUE     Escalator
    ## 1162                   NA     NA      NA      NA FALSE         Stair
    ## 1166                   NA     NA      NA      NA  TRUE         Stair
    ## 1170                   NA     NA      NA      NA  TRUE         Stair
    ## 1179                   NA     NA      NA      NA  TRUE         Stair
    ## 1183                   NA     NA      NA      NA  TRUE         Stair
    ## 1187                   NA     NA      NA      NA  TRUE         Stair
    ## 1190                   NA     NA      NA      NA  TRUE         Stair
    ## 1194                   NA     NA      NA      NA FALSE         Stair
    ## 1196                   NA     NA      NA      NA  TRUE         Stair
    ## 1200                   NA     NA      NA      NA  TRUE         Stair
    ## 1204                   NA     NA      NA      NA  TRUE         Stair
    ## 1208                   NA     NA      NA      NA  TRUE         Stair
    ## 1211                   NA     NA      NA      NA  TRUE         Stair
    ## 1215                   NA     NA      NA      NA  TRUE         Stair
    ## 1219                   NA     NA      NA      NA  TRUE         Stair
    ## 1222                   NA     NA      NA      NA  TRUE         Stair
    ## 1224                   NA     NA      NA      NA  TRUE         Stair
    ## 1227                   NA     NA      NA      NA  TRUE         Stair
    ## 1228                   NA     NA      NA      NA  TRUE         Stair
    ## 1232                   NA     NA      NA      NA  TRUE         Stair
    ## 1236                   NA     NA      NA      NA  TRUE         Stair
    ## 1239                   NA     NA      NA      NA  TRUE         Stair
    ## 1243                   NA     NA      NA      NA  TRUE         Stair
    ## 1245                   NA     NA      NA      NA  TRUE         Stair
    ## 1247                   NA     NA      NA      NA  TRUE      Elevator
    ## 1248                   NA     NA      NA      NA  TRUE         Stair
    ## 1251                   NA     NA      NA      NA  TRUE         Stair
    ## 1255                   NA     NA      NA      NA  TRUE         Stair
    ## 1256                   NA     NA      NA      NA  TRUE         Stair
    ## 1257                   NA     NA      NA      NA  TRUE         Stair
    ## 1261                   NA     NA      NA      NA  TRUE         Stair
    ## 1265                   NA     NA      NA      NA  TRUE         Stair
    ## 1269                   NA     NA      NA      NA  TRUE         Stair
    ## 1271                   NA     NA      NA      NA FALSE         Stair
    ## 1273                   NA     NA      NA      NA  TRUE          Door
    ## 1274                   NA     NA      NA      NA  TRUE         Stair
    ## 1276                   NA     NA      NA      NA  TRUE         Stair
    ## 1280                   NA     NA      NA      NA  TRUE         Stair
    ## 1284                   NA     NA      NA      NA  TRUE      Elevator
    ## 1285                   NA     NA      NA      NA  TRUE         Stair
    ## 1289      5      6     NA     NA      NA      NA  TRUE      Easement
    ## 1290      5      6     NA     NA      NA      NA  TRUE         Stair
    ## 1293      5      6     NA     NA      NA      NA  TRUE         Stair
    ## 1295                   NA     NA      NA      NA  TRUE         Stair
    ## 1298                   NA     NA      NA      NA  TRUE      Easement
    ## 1304                   NA     NA      NA      NA  TRUE         Stair
    ## 1308                   NA     NA      NA      NA  TRUE      Easement
    ## 1311                   NA     NA      NA      NA  TRUE         Stair
    ## 1320                   NA     NA      NA      NA  TRUE         Stair
    ## 1324                   NA     NA      NA      NA  TRUE      Easement
    ## 1329      6            NA     NA      NA      NA  TRUE         Stair
    ## 1330      6            NA     NA      NA      NA  TRUE      Easement
    ## 1335      6            NA     NA      NA      NA FALSE         Stair
    ## 1336                   NA     NA      NA      NA  TRUE          Door
    ## 1337                   NA     NA      NA      NA  TRUE         Stair
    ## 1341                   NA     NA      NA      NA  TRUE         Stair
    ## 1349                   NA     NA      NA      NA  TRUE         Stair
    ## 1357                   NA     NA      NA      NA  TRUE         Stair
    ## 1361                   NA     NA      NA      NA  TRUE         Stair
    ## 1363                   NA     NA      NA      NA  TRUE         Stair
    ## 1370                   NA     NA      NA      NA  TRUE         Stair
    ## 1375                   NA     NA      NA      NA  TRUE      Elevator
    ## 1376                   NA     NA      NA      NA  TRUE         Stair
    ## 1379                   NA     NA      NA      NA  TRUE         Stair
    ## 1380                   NA     NA      NA      NA  TRUE         Stair
    ## 1381                   NA     NA      NA      NA  TRUE         Stair
    ## 1382                   NA     NA      NA      NA  TRUE         Stair
    ## 1383                   NA     NA      NA      NA  TRUE         Stair
    ## 1384                   NA     NA      NA      NA  TRUE         Stair
    ## 1385                   NA     NA      NA      NA  TRUE         Stair
    ## 1386                   NA     NA      NA      NA  TRUE         Stair
    ## 1387                   NA     NA      NA      NA  TRUE         Stair
    ## 1389      6            NA     NA      NA      NA  TRUE         Stair
    ## 1391      6            NA     NA      NA      NA  TRUE      Elevator
    ## 1393      6            NA     NA      NA      NA FALSE      Elevator
    ## 1394      6            NA     NA      NA      NA FALSE         Stair
    ## 1396      3      4      5     NA      NA      NA  TRUE         Stair
    ## 1398      3      4      5     NA      NA      NA FALSE      Easement
    ## 1399      3      4      5     NA      NA      NA FALSE         Stair
    ## 1401                   NA     NA      NA      NA  TRUE      Easement
    ## 1404                   NA     NA      NA      NA  TRUE         Stair
    ## 1405                   NA     NA      NA      NA FALSE      Easement
    ## 1409                   NA     NA      NA      NA  TRUE         Stair
    ## 1413                   NA     NA      NA      NA  TRUE         Stair
    ## 1420                   NA     NA      NA      NA  TRUE         Stair
    ## 1421                   NA     NA      NA      NA  TRUE         Stair
    ## 1423                   NA     NA      NA      NA  TRUE         Stair
    ## 1427                   NA     NA      NA      NA  TRUE         Stair
    ## 1431                   NA     NA      NA      NA  TRUE         Stair
    ## 1433                   NA     NA      NA      NA  TRUE          Door
    ## 1434                   NA     NA      NA      NA  TRUE         Stair
    ## 1438                   NA     NA      NA      NA  TRUE         Stair
    ## 1442                   NA     NA      NA      NA  TRUE         Stair
    ## 1444                   NA     NA      NA      NA  TRUE         Stair
    ## 1446                   NA     NA      NA      NA  TRUE         Stair
    ## 1448                   NA     NA      NA      NA  TRUE         Stair
    ## 1450                   NA     NA      NA      NA  TRUE          Door
    ## 1451                   NA     NA      NA      NA  TRUE         Stair
    ## 1453                   NA     NA      NA      NA  TRUE         Stair
    ## 1455                   NA     NA      NA      NA  TRUE         Stair
    ## 1460                   NA     NA      NA      NA  TRUE      Easement
    ## 1464      6            NA     NA      NA      NA  TRUE         Stair
    ## 1465                   NA     NA      NA      NA  TRUE         Stair
    ## 1466                   NA     NA      NA      NA  TRUE         Stair
    ## 1467                   NA     NA      NA      NA  TRUE         Stair
    ## 1469      3      4      5     NA      NA      NA  TRUE      Easement
    ## 1472      3      4      5     NA      NA      NA  TRUE         Stair
    ## 1475                   NA     NA      NA      NA  TRUE         Stair
    ## 1477                   NA     NA      NA      NA  TRUE         Stair
    ## 1479                   NA     NA      NA      NA  TRUE         Stair
    ## 1481                   NA     NA      NA      NA  TRUE         Stair
    ## 1483                   NA     NA      NA      NA  TRUE         Stair
    ## 1486                   NA     NA      NA      NA  TRUE         Stair
    ## 1489                   NA     NA      NA      NA  TRUE         Stair
    ## 1491                   NA     NA      NA      NA  TRUE         Stair
    ## 1492                   NA     NA      NA      NA  TRUE         Stair
    ## 1493                   NA     NA      NA      NA  TRUE      Elevator
    ## 1494                   NA     NA      NA      NA  TRUE         Stair
    ## 1500                   NA     NA      NA      NA  TRUE      Elevator
    ## 1501                   NA     NA      NA      NA  TRUE         Stair
    ## 1508                   NA     NA      NA      NA  TRUE         Stair
    ## 1510                   NA     NA      NA      NA FALSE         Stair
    ## 1511                   NA     NA      NA      NA  TRUE         Stair
    ## 1513                   NA     NA      NA      NA  TRUE         Stair
    ## 1515                   NA     NA      NA      NA  TRUE         Stair
    ## 1516                   NA     NA      NA      NA  TRUE         Stair
    ## 1517                   NA     NA      NA      NA  TRUE         Stair
    ## 1521                   NA     NA      NA      NA  TRUE         Stair
    ## 1524                   NA     NA      NA      NA  TRUE         Stair
    ## 1528                   NA     NA      NA      NA  TRUE         Stair
    ## 1531                   NA     NA      NA      NA  TRUE         Stair
    ## 1533                   NA     NA      NA      NA  TRUE         Stair
    ## 1537                   NA     NA      NA      NA  TRUE         Stair
    ## 1541                   NA     NA      NA      NA  TRUE         Stair
    ## 1545                   NA     NA      NA      NA  TRUE         Stair
    ## 1547                   NA     NA      NA      NA  TRUE         Stair
    ## 1549                   NA     NA      NA      NA FALSE         Stair
    ## 1550                   NA     NA      NA      NA  TRUE         Stair
    ## 1554                   NA     NA      NA      NA  TRUE         Stair
    ## 1556                   NA     NA      NA      NA  TRUE         Stair
    ## 1559                   NA     NA      NA      NA  TRUE         Stair
    ## 1560                   NA     NA      NA      NA  TRUE          Door
    ## 1561                   NA     NA      NA      NA  TRUE      Elevator
    ## 1563                   NA     NA      NA      NA  TRUE         Stair
    ## 1565                   NA     NA      NA      NA  TRUE         Stair
    ## 1567                   NA     NA      NA      NA  TRUE         Stair
    ## 1568                   NA     NA      NA      NA  TRUE         Stair
    ## 1570                   NA     NA      NA      NA  TRUE         Stair
    ## 1572                   NA     NA      NA      NA  TRUE         Stair
    ## 1580                   NA     NA      NA      NA  TRUE      Easement
    ## 1581                   NA     NA      NA      NA  TRUE         Stair
    ## 1584                   NA     NA      NA      NA  TRUE         Stair
    ## 1587                   NA     NA      NA      NA  TRUE         Stair
    ## 1589                   NA     NA      NA      NA  TRUE         Stair
    ## 1593                   NA     NA      NA      NA  TRUE      Easement
    ## 1594                   NA     NA      NA      NA  TRUE         Stair
    ## 1596                   NA     NA      NA      NA  TRUE      Easement
    ## 1598                   NA     NA      NA      NA  TRUE         Stair
    ## 1600                   NA     NA      NA      NA FALSE         Stair
    ## 1605                   NA     NA      NA      NA  TRUE         Stair
    ## 1607                   NA     NA      NA      NA  TRUE         Stair
    ## 1611                   NA     NA      NA      NA  TRUE         Stair
    ## 1614                   NA     NA      NA      NA  TRUE         Stair
    ## 1618                   NA     NA      NA      NA  TRUE         Stair
    ## 1621                   NA     NA      NA      NA  TRUE         Stair
    ## 1626                   NA     NA      NA      NA  TRUE         Stair
    ## 1628                   NA     NA      NA      NA  TRUE         Stair
    ## 1631                   NA     NA      NA      NA  TRUE         Stair
    ## 1636                   NA     NA      NA      NA  TRUE          Door
    ## 1638                   NA     NA      NA      NA  TRUE         Stair
    ## 1640                   NA     NA      NA      NA FALSE         Stair
    ## 1643                   NA     NA      NA      NA  TRUE      Elevator
    ## 1644                   NA     NA      NA      NA  TRUE         Stair
    ## 1659                   NA     NA      NA      NA  TRUE      Elevator
    ## 1660                   NA     NA      NA      NA  TRUE         Stair
    ## 1667                   NA     NA      NA      NA  TRUE      Easement
    ## 1670                   NA     NA      NA      NA  TRUE      Elevator
    ## 1674                   NA     NA      NA      NA  TRUE         Stair
    ## 1676                   NA     NA      NA      NA  TRUE         Stair
    ## 1682                   NA     NA      NA      NA  TRUE         Stair
    ## 1689                   NA     NA      NA      NA  TRUE         Stair
    ## 1693                   NA     NA      NA      NA  TRUE         Stair
    ## 1698                   NA     NA      NA      NA  TRUE         Stair
    ## 1702                   NA     NA      NA      NA FALSE          Ramp
    ## 1703                   NA     NA      NA      NA  TRUE          Door
    ## 1704                   NA     NA      NA      NA FALSE         Stair
    ## 1705                   NA     NA      NA      NA  TRUE         Stair
    ## 1709                   NA     NA      NA      NA  TRUE         Stair
    ## 1712                   NA     NA      NA      NA  TRUE         Stair
    ## 1716                   NA     NA      NA      NA  TRUE         Stair
    ## 1720                   NA     NA      NA      NA  TRUE         Stair
    ## 1724                   NA     NA      NA      NA  TRUE         Stair
    ## 1728                   NA     NA      NA      NA  TRUE         Stair
    ## 1732                   NA     NA      NA      NA  TRUE         Stair
    ## 1735                   NA     NA      NA      NA  TRUE         Stair
    ## 1736                   NA     NA      NA      NA  TRUE          Door
    ## 1737                   NA     NA      NA      NA  TRUE      Elevator
    ## 1739                   NA     NA      NA      NA  TRUE         Stair
    ## 1740                   NA     NA      NA      NA  TRUE         Stair
    ## 1741                   NA     NA      NA      NA  TRUE          Door
    ## 1742                   NA     NA      NA      NA  TRUE          Door
    ## 1743                   NA     NA      NA      NA  TRUE          Door
    ## 1744                   NA     NA      NA      NA  TRUE          Door
    ## 1745                   NA     NA      NA      NA  TRUE          Door
    ## 1746                   NA     NA      NA      NA  TRUE          Door
    ## 1747                   NA     NA      NA      NA  TRUE          Door
    ## 1748                   NA     NA      NA      NA  TRUE          Door
    ## 1749                   NA     NA      NA      NA  TRUE          Door
    ## 1751                   NA     NA      NA      NA  TRUE          Door
    ## 1753                   NA     NA      NA      NA  TRUE          Door
    ## 1754                   NA     NA      NA      NA  TRUE          Door
    ## 1755                   NA     NA      NA      NA  TRUE          Door
    ## 1757                   NA     NA      NA      NA  TRUE         Stair
    ## 1760                   NA     NA      NA      NA  TRUE         Stair
    ## 1764                   NA     NA      NA      NA  TRUE         Stair
    ## 1768                   NA     NA      NA      NA  TRUE         Stair
    ## 1772                   NA     NA      NA      NA  TRUE         Stair
    ## 1775                   NA     NA      NA      NA  TRUE         Stair
    ## 1778                   NA     NA      NA      NA  TRUE         Stair
    ## 1782                   NA     NA      NA      NA FALSE         Stair
    ## 1784                   NA     NA      NA      NA  TRUE         Stair
    ## 1786                   NA     NA      NA      NA FALSE         Stair
    ## 1788                   NA     NA      NA      NA  TRUE          Door
    ## 1789                   NA     NA      NA      NA  TRUE         Stair
    ## 1793                   NA     NA      NA      NA  TRUE         Stair
    ## 1797                   NA     NA      NA      NA  TRUE         Stair
    ## 1801                   NA     NA      NA      NA  TRUE      Elevator
    ## 1802                   NA     NA      NA      NA  TRUE         Stair
    ## 1809                   NA     NA      NA      NA  TRUE         Stair
    ## 1813                   NA     NA      NA      NA  TRUE         Stair
    ## 1815                   NA     NA      NA      NA  TRUE         Stair
    ## 1818                   NA     NA      NA      NA  TRUE         Stair
    ## 1820                   NA     NA      NA      NA  TRUE         Stair
    ## 1822                   NA     NA      NA      NA  TRUE         Stair
    ## 1824                   NA     NA      NA      NA  TRUE         Stair
    ## 1827                   NA     NA      NA      NA  TRUE         Stair
    ## 1829                   NA     NA      NA      NA  TRUE          Door
    ## 1831                   NA     NA      NA      NA  TRUE     Escalator
    ## 1832                   NA     NA      NA      NA  TRUE         Stair
    ## 1836                   NA     NA      NA      NA  TRUE         Stair
    ## 1840                   NA     NA      NA      NA  TRUE          Door
    ## 1841                   NA     NA      NA      NA  TRUE         Stair
    ## 1842                   NA     NA      NA      NA  TRUE     Escalator
    ## 1843                   NA     NA      NA      NA  TRUE         Stair
    ## 1845                   NA     NA      NA      NA  TRUE         Stair
    ## 1849                   NA     NA      NA      NA  TRUE     Escalator
    ## 1850                   NA     NA      NA      NA  TRUE         Stair
    ## 1854                   NA     NA      NA      NA  TRUE         Stair
    ## 1859                   NA     NA      NA      NA  TRUE      Elevator
    ## 1860                   NA     NA      NA      NA  TRUE         Stair
    ## 1864                   NA     NA      NA      NA  TRUE         Stair
    ## 1867                   NA     NA      NA      NA  TRUE      Elevator
    ## 1868                   NA     NA      NA      NA  TRUE         Stair
    ##      vending   ada
    ## 1        YES FALSE
    ## 3        YES FALSE
    ## 6        YES FALSE
    ## 10       YES FALSE
    ## 14        NO FALSE
    ## 15       YES FALSE
    ## 21       YES FALSE
    ## 23        NO FALSE
    ## 24       YES FALSE
    ## 27       YES FALSE
    ## 32       YES FALSE
    ## 33        NO FALSE
    ## 34       YES  TRUE
    ## 35       YES FALSE
    ## 37        NO FALSE
    ## 38       YES  TRUE
    ## 39       YES  TRUE
    ## 42       YES  TRUE
    ## 44       YES  TRUE
    ## 46       YES FALSE
    ## 49       YES FALSE
    ## 53       YES  TRUE
    ## 57       YES  TRUE
    ## 58        NO  TRUE
    ## 59        NO  TRUE
    ## 60       YES FALSE
    ## 61       YES FALSE
    ## 71       YES FALSE
    ## 79       YES FALSE
    ## 83       YES  TRUE
    ## 84       YES  TRUE
    ## 91       YES  TRUE
    ## 93       YES FALSE
    ## 94       YES FALSE
    ## 102      YES  TRUE
    ## 108      YES  TRUE
    ## 109      YES  TRUE
    ## 113      YES  TRUE
    ## 119      YES FALSE
    ## 121      YES FALSE
    ## 129       NO FALSE
    ## 137      YES FALSE
    ## 141       NO FALSE
    ## 143      YES FALSE
    ## 146       NO FALSE
    ## 148      YES FALSE
    ## 153      YES  TRUE
    ## 159      YES FALSE
    ## 163      YES FALSE
    ## 167      YES FALSE
    ## 170      YES FALSE
    ## 171      YES FALSE
    ## 174      YES FALSE
    ## 180      YES FALSE
    ## 181      YES FALSE
    ## 182      YES  TRUE
    ## 183      YES  TRUE
    ## 185      YES  TRUE
    ## 187      YES  TRUE
    ## 188      YES  TRUE
    ## 190      YES  TRUE
    ## 192      YES  TRUE
    ## 193      YES FALSE
    ## 194      YES FALSE
    ## 198      YES FALSE
    ## 204      YES FALSE
    ## 209       NO FALSE
    ## 210      YES FALSE
    ## 216      YES  TRUE
    ## 217      YES  TRUE
    ## 221       NO  TRUE
    ## 224      YES FALSE
    ## 230      YES FALSE
    ## 233      YES  TRUE
    ## 237      YES  TRUE
    ## 238      YES  TRUE
    ## 244      YES FALSE
    ## 245      YES FALSE
    ## 249      YES FALSE
    ## 250      YES FALSE
    ## 251      YES FALSE
    ## 259       NO FALSE
    ## 262      YES  TRUE
    ## 263      YES  TRUE
    ## 278      YES  TRUE
    ## 280      YES  TRUE
    ## 285       NO  TRUE
    ## 287      YES  TRUE
    ## 290      YES  TRUE
    ## 291      YES  TRUE
    ## 292      YES  TRUE
    ## 296       NO  TRUE
    ## 297      YES  TRUE
    ## 299      YES  TRUE
    ## 300      YES  TRUE
    ## 302      YES  TRUE
    ## 307       NO  TRUE
    ## 309      YES FALSE
    ## 312      YES FALSE
    ## 314      YES FALSE
    ## 316      YES FALSE
    ## 320       NO FALSE
    ## 321      YES FALSE
    ## 324      YES FALSE
    ## 326      YES FALSE
    ## 331      YES FALSE
    ## 334      YES FALSE
    ## 342       NO FALSE
    ## 343      YES FALSE
    ## 345      YES FALSE
    ## 346       NO FALSE
    ## 350      YES FALSE
    ## 353      YES  TRUE
    ## 354      YES  TRUE
    ## 359      YES FALSE
    ## 361       NO FALSE
    ## 362      YES  TRUE
    ## 363      YES  TRUE
    ## 368      YES  TRUE
    ## 376      YES FALSE
    ## 377      YES FALSE
    ## 379      YES  TRUE
    ## 380      YES  TRUE
    ## 383      YES  TRUE
    ## 389      YES  TRUE
    ## 393      YES FALSE
    ## 397      YES FALSE
    ## 400      YES FALSE
    ## 402      YES FALSE
    ## 406      YES FALSE
    ## 409      YES FALSE
    ## 413      YES FALSE
    ## 415      YES  TRUE
    ## 416      YES FALSE
    ## 417       NO FALSE
    ## 418      YES FALSE
    ## 419       NO FALSE
    ## 420       NO FALSE
    ## 421      YES FALSE
    ## 422       NO FALSE
    ## 423      YES FALSE
    ## 424      YES FALSE
    ## 425      YES FALSE
    ## 426      YES FALSE
    ## 433      YES FALSE
    ## 435      YES FALSE
    ## 436      YES FALSE
    ## 439      YES FALSE
    ## 440      YES FALSE
    ## 441      YES FALSE
    ## 447      YES FALSE
    ## 448       NO FALSE
    ## 449      YES  TRUE
    ## 450      YES  TRUE
    ## 452      YES FALSE
    ## 454      YES  TRUE
    ## 455       NO FALSE
    ## 456      YES FALSE
    ## 457      YES FALSE
    ## 461       NO FALSE
    ## 464       NO FALSE
    ## 465      YES FALSE
    ## 469      YES  TRUE
    ## 473      YES  TRUE
    ## 475      YES  TRUE
    ## 480      YES FALSE
    ## 488      YES FALSE
    ## 492      YES FALSE
    ## 495      YES FALSE
    ## 497       NO FALSE
    ## 502       NO FALSE
    ## 503      YES FALSE
    ## 510      YES FALSE
    ## 513       NO FALSE
    ## 515      YES FALSE
    ## 516      YES FALSE
    ## 517      YES FALSE
    ## 520      YES FALSE
    ## 527      YES FALSE
    ## 531      YES FALSE
    ## 535      YES FALSE
    ## 538       NO FALSE
    ## 543       NO FALSE
    ## 544      YES  TRUE
    ## 548      YES  TRUE
    ## 551       NO  TRUE
    ## 552       NO FALSE
    ## 554      YES FALSE
    ## 558      YES FALSE
    ## 560      YES FALSE
    ## 562      YES FALSE
    ## 564       NO FALSE
    ## 565       NO FALSE
    ## 566      YES FALSE
    ## 568      YES FALSE
    ## 570      YES FALSE
    ## 572      YES FALSE
    ## 574      YES FALSE
    ## 577      YES FALSE
    ## 579      YES  TRUE
    ## 581      YES  TRUE
    ## 583      YES FALSE
    ## 585      YES FALSE
    ## 587      YES FALSE
    ## 589      YES FALSE
    ## 591      YES FALSE
    ## 593      YES FALSE
    ## 595       NO FALSE
    ## 597      YES  TRUE
    ## 598      YES  TRUE
    ## 601       NO  TRUE
    ## 605      YES FALSE
    ## 607      YES FALSE
    ## 609      YES FALSE
    ## 611      YES FALSE
    ## 614      YES FALSE
    ## 618       NO FALSE
    ## 619      YES FALSE
    ## 623       NO FALSE
    ## 624      YES FALSE
    ## 627      YES FALSE
    ## 628      YES FALSE
    ## 632      YES FALSE
    ## 635      YES FALSE
    ## 638       NO FALSE
    ## 642       NO FALSE
    ## 643      YES FALSE
    ## 647      YES FALSE
    ## 649       NO FALSE
    ## 650      YES FALSE
    ## 651      YES FALSE
    ## 652      YES FALSE
    ## 656       NO FALSE
    ## 658      YES FALSE
    ## 659      YES FALSE
    ## 660      YES FALSE
    ## 662       NO FALSE
    ## 663       NO FALSE
    ## 664      YES FALSE
    ## 666       NO FALSE
    ## 667       NO FALSE
    ## 668      YES  TRUE
    ## 670       NO  TRUE
    ## 672      YES FALSE
    ## 673       NO FALSE
    ## 675      YES FALSE
    ## 679      YES FALSE
    ## 681       NO FALSE
    ## 685      YES  TRUE
    ## 689      YES  TRUE
    ## 690       NO  TRUE
    ## 696      YES FALSE
    ## 700      YES FALSE
    ## 702      YES  TRUE
    ## 705       NO  TRUE
    ## 707      YES  TRUE
    ## 708      YES  TRUE
    ## 713      YES  TRUE
    ## 716       NO  TRUE
    ## 717      YES FALSE
    ## 721      YES FALSE
    ## 726      YES  TRUE
    ## 727      YES  TRUE
    ## 731      YES  TRUE
    ## 732      YES FALSE
    ## 736      YES FALSE
    ## 739      YES  TRUE
    ## 744      YES FALSE
    ## 749      YES FALSE
    ## 751      YES FALSE
    ## 752       NO FALSE
    ## 756      YES FALSE
    ## 764      YES FALSE
    ## 765       NO FALSE
    ## 766      YES FALSE
    ## 767       NO FALSE
    ## 771       NO FALSE
    ## 772       NO  TRUE
    ## 773      YES  TRUE
    ## 774       NO  TRUE
    ## 775      YES  TRUE
    ## 776       NO  TRUE
    ## 777      YES  TRUE
    ## 778      YES  TRUE
    ## 786      YES FALSE
    ## 790      YES FALSE
    ## 794      YES FALSE
    ## 798      YES FALSE
    ## 800      YES  TRUE
    ## 802      YES FALSE
    ## 805      YES FALSE
    ## 809      YES FALSE
    ## 810      YES  TRUE
    ## 811       NO  TRUE
    ## 812       NO  TRUE
    ## 813      YES FALSE
    ## 819       NO FALSE
    ## 821      YES FALSE
    ## 823      YES FALSE
    ## 827      YES FALSE
    ## 831      YES FALSE
    ## 835       NO FALSE
    ## 837      YES FALSE
    ## 841       NO FALSE
    ## 843      YES FALSE
    ## 844       NO FALSE
    ## 845      YES FALSE
    ## 847      YES FALSE
    ## 849      YES FALSE
    ## 851       NO FALSE
    ## 852      YES  TRUE
    ## 855      YES FALSE
    ## 856      YES FALSE
    ## 857      YES  TRUE
    ## 859      YES FALSE
    ## 860      YES  TRUE
    ## 861      YES  TRUE
    ## 864      YES FALSE
    ## 866      YES FALSE
    ## 867       NO FALSE
    ## 870      YES FALSE
    ## 871      YES FALSE
    ## 873      YES FALSE
    ## 877      YES FALSE
    ## 878      YES FALSE
    ## 883       NO FALSE
    ## 884      YES FALSE
    ## 889      YES FALSE
    ## 890      YES FALSE
    ## 893      YES FALSE
    ## 897      YES FALSE
    ## 902      YES FALSE
    ## 908      YES FALSE
    ## 910      YES FALSE
    ## 913      YES FALSE
    ## 917      YES FALSE
    ## 922      YES  TRUE
    ## 923      YES  TRUE
    ## 927      YES  TRUE
    ## 930      YES  TRUE
    ## 931      YES FALSE
    ## 933      YES FALSE
    ## 936      YES FALSE
    ## 942      YES FALSE
    ## 946      YES FALSE
    ## 949      YES FALSE
    ## 950       NO FALSE
    ## 957      YES FALSE
    ## 959      YES FALSE
    ## 961      YES FALSE
    ## 966      YES FALSE
    ## 967       NO FALSE
    ## 968       NO FALSE
    ## 969      YES FALSE
    ## 971       NO FALSE
    ## 973      YES FALSE
    ## 975      YES FALSE
    ## 979       NO FALSE
    ## 981      YES FALSE
    ## 985      YES FALSE
    ## 987       NO FALSE
    ## 989      YES FALSE
    ## 993      YES FALSE
    ## 995      YES FALSE
    ## 997       NO FALSE
    ## 999      YES FALSE
    ## 1001     YES FALSE
    ## 1004     YES FALSE
    ## 1008     YES FALSE
    ## 1012     YES FALSE
    ## 1014      NO FALSE
    ## 1015     YES FALSE
    ## 1016     YES FALSE
    ## 1017      NO FALSE
    ## 1018     YES FALSE
    ## 1019     YES FALSE
    ## 1020     YES FALSE
    ## 1021     YES  TRUE
    ## 1022     YES FALSE
    ## 1027     YES FALSE
    ## 1029     YES FALSE
    ## 1033     YES FALSE
    ## 1037     YES FALSE
    ## 1040      NO FALSE
    ## 1041      NO FALSE
    ## 1042     YES FALSE
    ## 1044     YES FALSE
    ## 1048     YES FALSE
    ## 1050     YES  TRUE
    ## 1051     YES  TRUE
    ## 1057     YES FALSE
    ## 1061     YES FALSE
    ## 1064     YES FALSE
    ## 1067     YES FALSE
    ## 1068      NO FALSE
    ## 1070     YES FALSE
    ## 1073     YES FALSE
    ## 1076     YES FALSE
    ## 1082     YES  TRUE
    ## 1083     YES FALSE
    ## 1085     YES  TRUE
    ## 1090     YES  TRUE
    ## 1091     YES  TRUE
    ## 1095     YES  TRUE
    ## 1096     YES FALSE
    ## 1099     YES  TRUE
    ## 1100     YES  TRUE
    ## 1104     YES FALSE
    ## 1108     YES FALSE
    ## 1112     YES FALSE
    ## 1113     YES FALSE
    ## 1116     YES FALSE
    ## 1117     YES FALSE
    ## 1118     YES FALSE
    ## 1124     YES FALSE
    ## 1130      NO FALSE
    ## 1132     YES  TRUE
    ## 1133     YES  TRUE
    ## 1135     YES FALSE
    ## 1136     YES  TRUE
    ## 1137     YES  TRUE
    ## 1139     YES FALSE
    ## 1140     YES FALSE
    ## 1143      NO FALSE
    ## 1144      NO FALSE
    ## 1145     YES  TRUE
    ## 1146     YES  TRUE
    ## 1150     YES  TRUE
    ## 1151     YES FALSE
    ## 1153     YES FALSE
    ## 1155     YES FALSE
    ## 1162      NO FALSE
    ## 1166     YES FALSE
    ## 1170     YES FALSE
    ## 1179     YES FALSE
    ## 1183     YES FALSE
    ## 1187     YES FALSE
    ## 1190     YES FALSE
    ## 1194      NO FALSE
    ## 1196     YES FALSE
    ## 1200     YES FALSE
    ## 1204     YES FALSE
    ## 1208     YES FALSE
    ## 1211     YES FALSE
    ## 1215     YES FALSE
    ## 1219     YES FALSE
    ## 1222     YES FALSE
    ## 1224     YES FALSE
    ## 1227     YES FALSE
    ## 1228     YES FALSE
    ## 1232     YES  TRUE
    ## 1236     YES FALSE
    ## 1239     YES FALSE
    ## 1243     YES FALSE
    ## 1245     YES FALSE
    ## 1247     YES  TRUE
    ## 1248     YES  TRUE
    ## 1251      NO  TRUE
    ## 1255     YES FALSE
    ## 1256      NO FALSE
    ## 1257     YES FALSE
    ## 1261     YES FALSE
    ## 1265     YES  TRUE
    ## 1269     YES FALSE
    ## 1271      NO FALSE
    ## 1273     YES FALSE
    ## 1274     YES FALSE
    ## 1276     YES FALSE
    ## 1280     YES FALSE
    ## 1284     YES  TRUE
    ## 1285     YES  TRUE
    ## 1289     YES FALSE
    ## 1290     YES FALSE
    ## 1293      NO FALSE
    ## 1295     YES FALSE
    ## 1298     YES FALSE
    ## 1304     YES FALSE
    ## 1308     YES FALSE
    ## 1311     YES FALSE
    ## 1320     YES  TRUE
    ## 1324     YES  TRUE
    ## 1329     YES FALSE
    ## 1330     YES FALSE
    ## 1335      NO FALSE
    ## 1336     YES FALSE
    ## 1337     YES FALSE
    ## 1341     YES FALSE
    ## 1349     YES FALSE
    ## 1357     YES FALSE
    ## 1361     YES FALSE
    ## 1363     YES FALSE
    ## 1370     YES  TRUE
    ## 1375     YES  TRUE
    ## 1376     YES  TRUE
    ## 1379      NO  TRUE
    ## 1380     YES  TRUE
    ## 1381     YES  TRUE
    ## 1382     YES  TRUE
    ## 1383     YES  TRUE
    ## 1384     YES  TRUE
    ## 1385     YES  TRUE
    ## 1386     YES  TRUE
    ## 1387     YES  TRUE
    ## 1389     YES  TRUE
    ## 1391     YES  TRUE
    ## 1393      NO  TRUE
    ## 1394      NO  TRUE
    ## 1396     YES FALSE
    ## 1398      NO FALSE
    ## 1399      NO FALSE
    ## 1401     YES  TRUE
    ## 1404     YES  TRUE
    ## 1405      NO  TRUE
    ## 1409     YES FALSE
    ## 1413     YES FALSE
    ## 1420      NO FALSE
    ## 1421     YES FALSE
    ## 1423     YES FALSE
    ## 1427     YES FALSE
    ## 1431     YES FALSE
    ## 1433     YES FALSE
    ## 1434     YES FALSE
    ## 1438     YES FALSE
    ## 1442     YES FALSE
    ## 1444     YES FALSE
    ## 1446     YES FALSE
    ## 1448     YES FALSE
    ## 1450     YES  TRUE
    ## 1451     YES FALSE
    ## 1453     YES FALSE
    ## 1455     YES FALSE
    ## 1460     YES FALSE
    ## 1464     YES FALSE
    ## 1465     YES FALSE
    ## 1466     YES FALSE
    ## 1467     YES FALSE
    ## 1469     YES FALSE
    ## 1472     YES FALSE
    ## 1475     YES FALSE
    ## 1477     YES FALSE
    ## 1479     YES FALSE
    ## 1481     YES FALSE
    ## 1483     YES FALSE
    ## 1486     YES FALSE
    ## 1489     YES FALSE
    ## 1491     YES FALSE
    ## 1492      NO FALSE
    ## 1493     YES  TRUE
    ## 1494     YES  TRUE
    ## 1500     YES  TRUE
    ## 1501     YES  TRUE
    ## 1508     YES FALSE
    ## 1510      NO FALSE
    ## 1511     YES FALSE
    ## 1513     YES FALSE
    ## 1515     YES FALSE
    ## 1516      NO FALSE
    ## 1517     YES FALSE
    ## 1521      NO FALSE
    ## 1524     YES FALSE
    ## 1528     YES FALSE
    ## 1531     YES FALSE
    ## 1533     YES FALSE
    ## 1537     YES FALSE
    ## 1541     YES FALSE
    ## 1545     YES FALSE
    ## 1547     YES FALSE
    ## 1549      NO FALSE
    ## 1550     YES FALSE
    ## 1554     YES FALSE
    ## 1556     YES FALSE
    ## 1559     YES FALSE
    ## 1560     YES  TRUE
    ## 1561     YES  TRUE
    ## 1563     YES  TRUE
    ## 1565     YES FALSE
    ## 1567     YES FALSE
    ## 1568     YES FALSE
    ## 1570     YES FALSE
    ## 1572     YES FALSE
    ## 1580     YES FALSE
    ## 1581     YES FALSE
    ## 1584     YES FALSE
    ## 1587      NO FALSE
    ## 1589     YES FALSE
    ## 1593     YES FALSE
    ## 1594     YES FALSE
    ## 1596      NO FALSE
    ## 1598     YES FALSE
    ## 1600      NO FALSE
    ## 1605     YES FALSE
    ## 1607     YES FALSE
    ## 1611     YES FALSE
    ## 1614     YES FALSE
    ## 1618     YES FALSE
    ## 1621      NO FALSE
    ## 1626     YES FALSE
    ## 1628     YES FALSE
    ## 1631     YES FALSE
    ## 1636     YES  TRUE
    ## 1638     YES  TRUE
    ## 1640      NO  TRUE
    ## 1643     YES  TRUE
    ## 1644     YES  TRUE
    ## 1659     YES  TRUE
    ## 1660     YES  TRUE
    ## 1667     YES  TRUE
    ## 1670     YES  TRUE
    ## 1674     YES FALSE
    ## 1676     YES FALSE
    ## 1682     YES  TRUE
    ## 1689     YES FALSE
    ## 1693     YES FALSE
    ## 1698     YES FALSE
    ## 1702      NO FALSE
    ## 1703     YES FALSE
    ## 1704      NO FALSE
    ## 1705     YES FALSE
    ## 1709     YES FALSE
    ## 1712     YES FALSE
    ## 1716     YES FALSE
    ## 1720     YES FALSE
    ## 1724     YES FALSE
    ## 1728     YES FALSE
    ## 1732     YES FALSE
    ## 1735     YES FALSE
    ## 1736     YES FALSE
    ## 1737     YES  TRUE
    ## 1739     YES  TRUE
    ## 1740      NO  TRUE
    ## 1741     YES  TRUE
    ## 1742     YES FALSE
    ## 1743      NO FALSE
    ## 1744     YES FALSE
    ## 1745     YES FALSE
    ## 1746     YES FALSE
    ## 1747     YES FALSE
    ## 1748      NO FALSE
    ## 1749     YES FALSE
    ## 1751     YES FALSE
    ## 1753     YES FALSE
    ## 1754      NO FALSE
    ## 1755     YES FALSE
    ## 1757     YES FALSE
    ## 1760     YES FALSE
    ## 1764     YES FALSE
    ## 1768     YES FALSE
    ## 1772     YES FALSE
    ## 1775     YES FALSE
    ## 1778     YES FALSE
    ## 1782      NO FALSE
    ## 1784     YES FALSE
    ## 1786      NO FALSE
    ## 1788     YES FALSE
    ## 1789     YES FALSE
    ## 1793     YES FALSE
    ## 1797     YES FALSE
    ## 1801     YES  TRUE
    ## 1802     YES  TRUE
    ## 1809     YES FALSE
    ## 1813     YES FALSE
    ## 1815     YES FALSE
    ## 1818     YES  TRUE
    ## 1820     YES FALSE
    ## 1822     YES FALSE
    ## 1824     YES FALSE
    ## 1827     YES FALSE
    ## 1829     YES FALSE
    ## 1831     YES FALSE
    ## 1832     YES FALSE
    ## 1836     YES FALSE
    ## 1840     YES FALSE
    ## 1841     YES FALSE
    ## 1842     YES FALSE
    ## 1843     YES FALSE
    ## 1845     YES FALSE
    ## 1849     YES  TRUE
    ## 1850     YES  TRUE
    ## 1854     YES FALSE
    ## 1859     YES  TRUE
    ## 1860     YES  TRUE
    ## 1864     YES FALSE
    ## 1867     YES  TRUE
    ## 1868     YES  TRUE

``` r
# The dataset contains information regarding NYC subway stations including, the line name,
# its location, its various routes, its entrance type, whether it has vending, whether it
# allows entry, and whether or not it is ADA compliant. I cleaned the dataset by converting # the names into snake case and taking out superfluous columns such as the coordinates for 
# the entrance. The new data set contained 684 rows and 19 columns. After the instructed  
# tidying process, the dataset is relatively tidy, but there is still room for improvement. # For example, some stations are repeated. This was solved by using the unique function. 

dim(transit_data) # show rows and columns of data
```

    ## [1] 684  19

``` r
transit_data %>%
  distinct(line, station_name)  # Count number of distinct stations
```

    ##                  line                            station_name
    ## 1            4 Avenue                                 25th St
    ## 2            4 Avenue                                 36th St
    ## 3            4 Avenue                                 45th St
    ## 4            4 Avenue                                 53rd St
    ## 5            4 Avenue                                 59th St
    ## 6            4 Avenue                                 77th St
    ## 7            4 Avenue                                 86th St
    ## 8            4 Avenue                                 95th St
    ## 9            4 Avenue                                  9th St
    ## 10           4 Avenue                Atlantic Av-Barclays Ctr
    ## 11           4 Avenue                            Bay Ridge Av
    ## 12           4 Avenue                               DeKalb Av
    ## 13           4 Avenue                              Pacific St
    ## 14           4 Avenue                             Prospect Av
    ## 15           4 Avenue                                Union St
    ## 16    42nd St Shuttle                           Grand Central
    ## 17    42nd St Shuttle                            Times Square
    ## 18           6 Avenue                                 14th St
    ## 19           6 Avenue                                 23rd St
    ## 20           6 Avenue                                  2nd Av
    ## 21           6 Avenue                                 34th St
    ## 22           6 Avenue                                 42nd St
    ## 23           6 Avenue          47-50th Sts Rockefeller Center
    ## 24           6 Avenue                                  4th Av
    ## 25           6 Avenue                                 57th St
    ## 26           6 Avenue                                  7th Av
    ## 27           6 Avenue                               Bergen St
    ## 28           6 Avenue                   Broadway-Lafayette St
    ## 29           6 Avenue                              Carroll St
    ## 30           6 Avenue                               Church Av
    ## 31           6 Avenue                             Delancey St
    ## 32           6 Avenue                           East Broadway
    ## 33           6 Avenue                   Fort Hamilton Parkway
    ## 34           6 Avenue                                Grand St
    ## 35           6 Avenue                     Prospect Park-15 St
    ## 36           6 Avenue                            Smith-9th St
    ## 37           6 Avenue                                 York St
    ## 38        63rd Street                                 21st St
    ## 39        63rd Street                            Lexington Av
    ## 40        63rd Street                        Roosevelt Island
    ## 41           8 Avenue                                103rd St
    ## 42           8 Avenue                                116th St
    ## 43           8 Avenue                                125th St
    ## 44           8 Avenue                                135th St
    ## 45           8 Avenue                                145th St
    ## 46           8 Avenue                                 14th St
    ## 47           8 Avenue                                155th St
    ## 48           8 Avenue                 163rd St - Amsterdam Av
    ## 49           8 Avenue           168th St - Washington Heights
    ## 50           8 Avenue                                175th St
    ## 51           8 Avenue                                181st St
    ## 52           8 Avenue                                190th St
    ## 53           8 Avenue                                 23rd St
    ## 54           8 Avenue                                 34th St
    ## 55           8 Avenue                                 42nd St
    ## 56           8 Avenue                                 50th St
    ## 57           8 Avenue                                 59th St
    ## 58           8 Avenue                                 72nd St
    ## 59           8 Avenue     81st St - Museum of Natural History
    ## 60           8 Avenue                                 86th St
    ## 61           8 Avenue                                 96th St
    ## 62           8 Avenue                         Broadway-Nassau
    ## 63           8 Avenue                                Canal St
    ## 64           8 Avenue              Cathedral Parkway-110th St
    ## 65           8 Avenue                             Chambers St
    ## 66           8 Avenue                     Dyckman St-200th St
    ## 67           8 Avenue                                 High St
    ## 68           8 Avenue                       Inwood - 207th St
    ## 69           8 Avenue                               Spring St
    ## 70           8 Avenue                             West 4th St
    ## 71           8 Avenue                      World Trade Center
    ## 72          Archer Av                        Jamaica-Van Wyck
    ## 73          Archer Av Parsons Blvd-Archer Av - Jamaica Center
    ## 74          Archer Av            Sutphin Blvd-Archer Av - JFK
    ## 75            Astoria                          30 Av-Grand Av
    ## 76            Astoria                     36 Av-Washington Av
    ## 77            Astoria                          39 Av-Beebe Av
    ## 78            Astoria                    Astoria Blvd-Hoyt Av
    ## 79            Astoria                                Broadway
    ## 80            Astoria                            Ditmars Blvd
    ## 81           Brighton                                  7th Av
    ## 82           Brighton                             Atlantic Av
    ## 83           Brighton                                    Av H
    ## 84           Brighton                                    Av J
    ## 85           Brighton                                    Av M
    ## 86           Brighton                                    Av U
    ## 87           Brighton                              Beverly Rd
    ## 88           Brighton                          Brighton Beach
    ## 89           Brighton                               Church Av
    ## 90           Brighton                            Cortelyou Rd
    ## 91           Brighton                           Kings Highway
    ## 92           Brighton                                 Neck Rd
    ## 93           Brighton                              Newkirk Av
    ## 94           Brighton                           Ocean Parkway
    ## 95           Brighton                             Parkside Av
    ## 96           Brighton                           Prospect Park
    ## 97           Brighton                          Sheepshead Bay
    ## 98           Brighton                            Stillwell Av
    ## 99           Brighton                             West 8th St
    ## 100          Broadway                                 23rd St
    ## 101          Broadway                                 28th St
    ## 102          Broadway                                 34th St
    ## 103          Broadway                                 49th St
    ## 104          Broadway                                 57th St
    ## 105          Broadway                                  5th Av
    ## 106          Broadway                                  8th St
    ## 107          Broadway                           Canal St (UL)
    ## 108          Broadway                               City Hall
    ## 109          Broadway                            Cortlandt St
    ## 110          Broadway                                Court St
    ## 111          Broadway                             Lawrence St
    ## 112          Broadway                            Lexington Av
    ## 113          Broadway                               Prince St
    ## 114          Broadway                               Rector St
    ## 115          Broadway                    Times Square-42nd St
    ## 116          Broadway                            Union Square
    ## 117          Broadway                            Whitehall St
    ## 118  Broadway Jamaica                       104th St-102nd St
    ## 119  Broadway Jamaica                                111th St
    ## 120  Broadway Jamaica                                121st St
    ## 121  Broadway Jamaica                              Alabama Av
    ## 122  Broadway Jamaica                             Chauncey St
    ## 123  Broadway Jamaica                            Cleveland St
    ## 124  Broadway Jamaica                             Crescent St
    ## 125  Broadway Jamaica                           Cypress Hills
    ## 126  Broadway Jamaica                    Elderts Lane-75th St
    ## 127  Broadway Jamaica                             Flushing Av
    ## 128  Broadway Jamaica                  Forest Parkway-85th St
    ## 129  Broadway Jamaica                                Gates Av
    ## 130  Broadway Jamaica                               Halsey St
    ## 131  Broadway Jamaica                                Hewes St
    ## 132  Broadway Jamaica                            Kosciusko St
    ## 133  Broadway Jamaica                              Lorimer St
    ## 134  Broadway Jamaica                                Marcy Av
    ## 135  Broadway Jamaica                               Myrtle Av
    ## 136  Broadway Jamaica                              Norwood Av
    ## 137  Broadway Jamaica                           Van Siclen Av
    ## 138  Broadway Jamaica                          Woodhaven Blvd
    ## 139  Broadway-7th Ave                                103rd St
    ## 140  Broadway-7th Ave            116th St-Columbia University
    ## 141  Broadway-7th Ave                                125th St
    ## 142  Broadway-7th Ave                   137th St-City College
    ## 143  Broadway-7th Ave                                145th St
    ## 144  Broadway-7th Ave                                 14th St
    ## 145  Broadway-7th Ave                                157th St
    ## 146  Broadway-7th Ave                                168th St
    ## 147  Broadway-7th Ave                                181st St
    ## 148  Broadway-7th Ave                                 18th St
    ## 149  Broadway-7th Ave                                191st St
    ## 150  Broadway-7th Ave                                207th St
    ## 151  Broadway-7th Ave                                215th St
    ## 152  Broadway-7th Ave                                231st St
    ## 153  Broadway-7th Ave                                238th St
    ## 154  Broadway-7th Ave                                 23rd St
    ## 155  Broadway-7th Ave                                 28th St
    ## 156  Broadway-7th Ave                                 34th St
    ## 157  Broadway-7th Ave                                 50th St
    ## 158  Broadway-7th Ave                 59th St-Columbus Circle
    ## 159  Broadway-7th Ave                  66th St-Lincoln Center
    ## 160  Broadway-7th Ave                                 72nd St
    ## 161  Broadway-7th Ave                                 79th St
    ## 162  Broadway-7th Ave                                 86th St
    ## 163  Broadway-7th Ave                                 96th St
    ## 164  Broadway-7th Ave                                Canal St
    ## 165  Broadway-7th Ave              Cathedral Parkway-110th St
    ## 166  Broadway-7th Ave                             Chambers St
    ## 167  Broadway-7th Ave                          Christopher St
    ## 168  Broadway-7th Ave                              Dyckman St
    ## 169  Broadway-7th Ave                             Franklin St
    ## 170  Broadway-7th Ave                              Houston St
    ## 171  Broadway-7th Ave                    Marble Hill-225th St
    ## 172  Broadway-7th Ave                               Rector St
    ## 173  Broadway-7th Ave                             South Ferry
    ## 174  Broadway-7th Ave                            Times Square
    ## 175  Broadway-7th Ave             Van Cortlandt Park-242nd St
    ## 176          Canarsie                                  1st Av
    ## 177          Canarsie                                  3rd Av
    ## 178          Canarsie                                  6th Av
    ## 179          Canarsie                                  8th Av
    ## 180          Canarsie                             Atlantic Av
    ## 181          Canarsie                              Bedford Av
    ## 182          Canarsie                             Bushwick Av
    ## 183          Canarsie             Canarsie - Rockaway Parkway
    ## 184          Canarsie                               DeKalb Av
    ## 185          Canarsie                           East 105th St
    ## 186          Canarsie                               Graham Av
    ## 187          Canarsie                                Grand St
    ## 188          Canarsie                               Halsey St
    ## 189          Canarsie                            Jefferson St
    ## 190          Canarsie                              Livonia Av
    ## 191          Canarsie                              Lorimer St
    ## 192          Canarsie                             Montrose Av
    ## 193          Canarsie                               Morgan Av
    ## 194          Canarsie                               Myrtle Av
    ## 195          Canarsie                             New Lots Av
    ## 196          Canarsie                               Sutter Av
    ## 197          Canarsie                            Union Square
    ## 198          Canarsie                               Wilson Av
    ## 199             Clark                            Borough Hall
    ## 200             Clark                                Clark St
    ## 201             Clark                               Fulton St
    ## 202             Clark                              Park Place
    ## 203             Clark                                 Wall St
    ## 204         Concourse                                155th St
    ## 205         Concourse                                167th St
    ## 206         Concourse                                170th St
    ## 207         Concourse                           174-175th Sts
    ## 208         Concourse                         182nd-183rd Sts
    ## 209         Concourse                       Bedford Park Blvd
    ## 210         Concourse                              Fordham Rd
    ## 211         Concourse                          Kingsbridge Rd
    ## 212         Concourse                        Norwood-205th St
    ## 213         Concourse                              Tremont Av
    ## 214         Concourse                 Yankee Stadium-161st St
    ## 215      Coney Island                            Stillwell Av
    ## 216      Coney Island                             West 8th St
    ## 217         Crosstown                                 21st St
    ## 218         Crosstown                    Bedford-Nostrand Avs
    ## 219         Crosstown                                Broadway
    ## 220         Crosstown                              Classon Av
    ## 221         Crosstown                  Clinton-Washington Avs
    ## 222         Crosstown                             Flushing Av
    ## 223         Crosstown                               Fulton St
    ## 224         Crosstown                           Greenpoint Av
    ## 225         Crosstown           Long Island City-Court Square
    ## 226         Crosstown                         Metropolitan Av
    ## 227         Crosstown                   Myrtle-Willoughby Avs
    ## 228         Crosstown                               Nassau Av
    ## 229            Culver                                 18th Av
    ## 230            Culver                                    Av I
    ## 231            Culver                                    Av N
    ## 232            Culver                                    Av P
    ## 233            Culver                                    Av U
    ## 234            Culver                                    Av X
    ## 235            Culver                     Bay Parkway-22nd Av
    ## 236            Culver                               Ditmas Av
    ## 237            Culver                           Kings Highway
    ## 238            Culver                   Neptune Av-Van Siclen
    ## 239           Dyre Av                           Baychester Av
    ## 240           Dyre Av                     Eastchester-Dyre Av
    ## 241           Dyre Av                             Gun Hill Rd
    ## 242           Dyre Av                             Morris Park
    ## 243           Dyre Av                          Pelham Parkway
    ## 244   Eastern Parkway                Atlantic Av-Barclays Ctr
    ## 245   Eastern Parkway                               Bergen St
    ## 246   Eastern Parkway         Eastern Parkway-Brooklyn Museum
    ## 247   Eastern Parkway                             Franklin Av
    ## 248   Eastern Parkway                        Grand Army Plaza
    ## 249   Eastern Parkway                                 Hoyt St
    ## 250   Eastern Parkway                             Kingston Av
    ## 251   Eastern Parkway                               Nevins St
    ## 252   Eastern Parkway                             Nostrand Av
    ## 253   Eastern Parkway                                Utica Av
    ## 254          Flushing                                103rd St
    ## 255          Flushing                                111th St
    ## 256          Flushing                    45 Rd-Court House Sq
    ## 257          Flushing                                  5th Av
    ## 258          Flushing                 82nd St-Jackson Heights
    ## 259          Flushing                        90th St Elmhurst
    ## 260          Flushing                        Bliss St-46th St
    ## 261          Flushing                        Broadway-74th St
    ## 262          Flushing                         Fisk Av-69th St
    ## 263          Flushing                        Flushing-Main St
    ## 264          Flushing                   Grand Central-42nd St
    ## 265          Flushing                           Hunters Point
    ## 266          Flushing                           Junction Blvd
    ## 267          Flushing                      Lincoln Av-52nd St
    ## 268          Flushing                       Lowery St-40th St
    ## 269          Flushing                    Mets - Willets Point
    ## 270          Flushing                        Queensboro Plaza
    ## 271          Flushing                       Rawson St-33rd St
    ## 272          Flushing                  Vernon Blvd-Jackson Av
    ## 273          Flushing                     Woodside Av-61st St
    ## 274          Franklin                         Botanic Gardens
    ## 275          Franklin                             Franklin Av
    ## 276          Franklin                              Park Place
    ## 277            Fulton         Broadway Junction-East New York
    ## 278            Fulton                Clinton & Washington Avs
    ## 279            Fulton                               Euclid Av
    ## 280            Fulton                             Franklin Av
    ## 281            Fulton                     Hoyt & Schermerhorn
    ## 282            Fulton                   Jay St - Borough Hall
    ## 283            Fulton                         Kingston-Throop
    ## 284            Fulton                            Lafayette Av
    ## 285            Fulton                              Liberty Av
    ## 286            Fulton                             Nostrand Av
    ## 287            Fulton                                Ralph Av
    ## 288            Fulton                             Rockaway Av
    ## 289            Fulton                             Shepherd Av
    ## 290            Fulton                                Utica Av
    ## 291            Fulton                           Van Siclen Av
    ## 292            Jerome                                138th St
    ## 293            Jerome                149th St-Grand Concourse
    ## 294            Jerome                                167th St
    ## 295            Jerome                                170th St
    ## 296            Jerome                                176th St
    ## 297            Jerome                                183rd St
    ## 298            Jerome        Bedford Park Blvd-Lehman College
    ## 299            Jerome                             Burnside Av
    ## 300            Jerome                              Fordham Rd
    ## 301            Jerome                          Kingsbridge Rd
    ## 302            Jerome                         Mosholu Parkway
    ## 303            Jerome                              Mt Eden Av
    ## 304            Jerome                                Woodlawn
    ## 305            Jerome                 Yankee Stadium-161st St
    ## 306             Lenox             110th St-Central Park North
    ## 307             Lenox                                116th St
    ## 308             Lenox                                125th St
    ## 309             Lenox                                135th St
    ## 310             Lenox                                145th St
    ## 311             Lenox                         Harlem-148th St
    ## 312         Lexington                                103rd St
    ## 313         Lexington                                110th St
    ## 314         Lexington                                116th St
    ## 315         Lexington                                125th St
    ## 316         Lexington                    14th St-Union Square
    ## 317         Lexington                                 23rd St
    ## 318         Lexington                                 28th St
    ## 319         Lexington                                 33rd St
    ## 320         Lexington                                 51st St
    ## 321         Lexington                                 59th St
    ## 322         Lexington                  68th St-Hunter College
    ## 323         Lexington                                 77th St
    ## 324         Lexington                                 86th St
    ## 325         Lexington                                 96th St
    ## 326         Lexington                             Astor Place
    ## 327         Lexington                             Bleecker St
    ## 328         Lexington                            Borough Hall
    ## 329         Lexington                           Bowling Green
    ## 330         Lexington               Brooklyn Bridge-City Hall
    ## 331         Lexington                                Canal St
    ## 332         Lexington                               Fulton St
    ## 333         Lexington                   Grand Central-42nd St
    ## 334         Lexington                               Spring St
    ## 335         Lexington                                 Wall St
    ## 336           Liberty                      104th St-Oxford Av
    ## 337           Liberty                   111th St-Greenwood Av
    ## 338           Liberty                       80th St-Hudson St
    ## 339           Liberty                         88th St-Boyd Av
    ## 340           Liberty                                Grant Av
    ## 341           Liberty                           Lefferts Blvd
    ## 342           Liberty                           Rockaway Blvd
    ## 343            Myrtle                              Central Av
    ## 344            Myrtle                               Forest Av
    ## 345            Myrtle                           Fresh Pond Rd
    ## 346            Myrtle                        Knickerbocker Av
    ## 347            Myrtle                         Metropolitan Av
    ## 348            Myrtle                               Seneca Av
    ## 349            Nassau                                  Bowery
    ## 350            Nassau                                Broad St
    ## 351            Nassau                                Canal St
    ## 352            Nassau                             Chambers St
    ## 353            Nassau                                Essex St
    ## 354            Nassau                               Fulton St
    ## 355          New Lots                               Junius St
    ## 356          New Lots                             New Lots Av
    ## 357          New Lots                         Pennsylvania Av
    ## 358          New Lots                             Rockaway Av
    ## 359          New Lots                             Saratoga Av
    ## 360          New Lots                               Sutter Av
    ## 361          New Lots                           Van Siclen Av
    ## 362          Nostrand                              Beverly Rd
    ## 363          Nostrand                               Church Av
    ## 364          Nostrand            Flatbush Av-Brooklyn College
    ## 365          Nostrand                              Newkirk Av
    ## 366          Nostrand                            President St
    ## 367          Nostrand                             Sterling St
    ## 368          Nostrand                             Winthrop St
    ## 369            Pelham                        138th St-3rd Ave
    ## 370            Pelham                                Brook Av
    ## 371            Pelham                                Buhre Av
    ## 372            Pelham                          Castle Hill Av
    ## 373            Pelham                              Cypress Av
    ## 374            Pelham              East 143rd St-St Mary's St
    ## 375            Pelham                           East 149th St
    ## 376            Pelham                                Elder Av
    ## 377            Pelham                          Hunts Point Av
    ## 378            Pelham                             Longwood Av
    ## 379            Pelham                           Middletown Rd
    ## 380            Pelham                Morrison Av-Soundview Av
    ## 381            Pelham               Parkchester-East 177th St
    ## 382            Pelham                         Pelham Bay Park
    ## 383            Pelham                          St Lawrence Av
    ## 384            Pelham      Westchester Square-East Tremont Av
    ## 385            Pelham                             Whitlock Av
    ## 386            Pelham                               Zerega Av
    ## 387  Queens Boulevard                                169th St
    ## 388  Queens Boulevard                          23rd St-Ely Av
    ## 389  Queens Boulevard                                 36th St
    ## 390  Queens Boulevard                                 46th St
    ## 391  Queens Boulevard                          5th Av-53rd St
    ## 392  Queens Boulevard                    63rd Drive-Rego Park
    ## 393  Queens Boulevard                                 65th St
    ## 394  Queens Boulevard                                 67th Av
    ## 395  Queens Boulevard                                 75th Av
    ## 396  Queens Boulevard                                  7th Av
    ## 397  Queens Boulevard                 Briarwood-Van Wyck Blvd
    ## 398  Queens Boulevard                             Elmhurst Av
    ## 399  Queens Boulevard                    Forest Hills-71st Av
    ## 400  Queens Boulevard                        Grand Av-Newtown
    ## 401  Queens Boulevard           Jackson Heights-Roosevelt Ave
    ## 402  Queens Boulevard                        Jamaica-179th St
    ## 403  Queens Boulevard              Kew Gardens-Union Turnpike
    ## 404  Queens Boulevard                    Lexington Av-53rd St
    ## 405  Queens Boulevard                           Northern Blvd
    ## 406  Queens Boulevard                            Parsons Blvd
    ## 407  Queens Boulevard                            Queens Plaza
    ## 408  Queens Boulevard                             Steinway St
    ## 409  Queens Boulevard                            Sutphin Blvd
    ## 410  Queens Boulevard                          Woodhaven Blvd
    ## 411          Rockaway                      Aqueduct Racetrack
    ## 412          Rockaway               Aqueduct-North Conduit Av
    ## 413          Rockaway                          Beach 105th St
    ## 414          Rockaway                           Beach 25th St
    ## 415          Rockaway                           Beach 36th St
    ## 416          Rockaway                           Beach 44th St
    ## 417          Rockaway                           Beach 60th St
    ## 418          Rockaway                           Beach 67th St
    ## 419          Rockaway                           Beach 90th St
    ## 420          Rockaway                           Beach 98th St
    ## 421          Rockaway                           Broad Channel
    ## 422          Rockaway                    Far Rockaway-Mott Av
    ## 423          Rockaway                            Howard Beach
    ## 424          Rockaway               Rockaway Park-Beach 116th
    ## 425         Sea Beach                                 18th Av
    ## 426         Sea Beach                                 20th Av
    ## 427         Sea Beach                                 86th St
    ## 428         Sea Beach                                  8th Av
    ## 429         Sea Beach                                    Av U
    ## 430         Sea Beach                     Bay Parkway-22nd Av
    ## 431         Sea Beach                   Fort Hamilton Parkway
    ## 432         Sea Beach                           Kings Highway
    ## 433         Sea Beach                          New Utrecht Av
    ## 434          West End                                 18th Av
    ## 435          West End                                 20th Av
    ## 436          West End                                 25th Av
    ## 437          West End                                 50th St
    ## 438          West End                                 55th St
    ## 439          West End                                 62nd St
    ## 440          West End                                 71st St
    ## 441          West End                                 79th St
    ## 442          West End                                  9th Av
    ## 443          West End                             Bay 50th St
    ## 444          West End                             Bay Parkway
    ## 445          West End                   Fort Hamilton Parkway
    ## 446 White Plains Road                         149th St-3rd Av
    ## 447 White Plains Road                                174th St
    ## 448 White Plains Road                                219th St
    ## 449 White Plains Road                                225th St
    ## 450 White Plains Road                                233rd St
    ## 451 White Plains Road                      238th St-Nereid Av
    ## 452 White Plains Road                             Allerton Av
    ## 453 White Plains Road                         Bronx Park East
    ## 454 White Plains Road                                Burke Av
    ## 455 White Plains Road                           East 180th St
    ## 456 White Plains Road           East Tremont Av-West Farms Sq
    ## 457 White Plains Road                              Freeman St
    ## 458 White Plains Road                             Gun Hill Rd
    ## 459 White Plains Road                            Intervale Av
    ## 460 White Plains Road                              Jackson Av
    ## 461 White Plains Road                          Pelham Parkway
    ## 462 White Plains Road                             Prospect Av
    ## 463 White Plains Road                              Simpson St
    ## 464 White Plains Road                      Wakefield-241st St
    ## 465          Flushing                      34 St Hudson Yards

``` r
transit_data %>%
  distinct(line, station_name, ada) %>%
  summarize(sum(ada == "TRUE")) # Count how many stations are ADA compliant
```

    ##   sum(ada == "TRUE")
    ## 1                 84

``` r
transit_data %>%
    filter(vending == "NO") %>%  # show only entrances without vending 
  summarize(total = n(), yes = sum(entry == "TRUE")) %>% # find number of allowed entries
  mutate(prop = yes / total) # find proportion 
```

    ##   total yes      prop
    ## 1   117  45 0.3846154

Problem 2
---------

``` r
library(readxl)

wheel_data = read_excel("./hw2data/HealthyHarborWaterWheelTotals2018-7-28.xlsx", 
      sheet = 1, range = cell_cols("A:N")) %>%
      janitor::clean_names() %>% # import data and clean names
    filter(!is.na(dumpster)) %>% # filter out non dumpster specific data 
    mutate(sports_balls = as.integer(round(sports_balls))) # rounding sports balls 
   
wheel_data
```

    ## # A tibble: 285 x 14
    ##    dumpster month  year date                weight_tons volume_cubic_ya~
    ##       <dbl> <chr> <dbl> <dttm>                    <dbl>            <dbl>
    ##  1        1 May    2014 2014-05-16 00:00:00        4.31               18
    ##  2        2 May    2014 2014-05-16 00:00:00        2.74               13
    ##  3        3 May    2014 2014-05-16 00:00:00        3.45               15
    ##  4        4 May    2014 2014-05-17 00:00:00        3.1                15
    ##  5        5 May    2014 2014-05-17 00:00:00        4.06               18
    ##  6        6 May    2014 2014-05-20 00:00:00        2.71               13
    ##  7        7 May    2014 2014-05-21 00:00:00        1.91                8
    ##  8        8 May    2014 2014-05-28 00:00:00        3.7                16
    ##  9        9 June   2014 2014-06-05 00:00:00        2.52               14
    ## 10       10 June   2014 2014-06-11 00:00:00        3.76               18
    ## # ... with 275 more rows, and 8 more variables: plastic_bottles <dbl>,
    ## #   polystyrene <dbl>, cigarette_butts <dbl>, glass_bottles <dbl>,
    ## #   grocery_bags <dbl>, chip_bags <dbl>, sports_balls <int>,
    ## #   homes_powered <dbl>

``` r
prcp_2018 = read_excel("./hw2data/HealthyHarborWaterWheelTotals2018-7-28.xlsx",
                       sheet = "2018 Precipitation", skip = 1 ) %>%  # import data
  janitor::clean_names() %>%  # clean name
  na.omit() %>% # remove na  
  mutate(year = 2018) # put in year column

prcp_2018
```

    ## # A tibble: 7 x 3
    ##   month total  year
    ##   <dbl> <dbl> <dbl>
    ## 1     1  0.96  2018
    ## 2     2  5.3   2018
    ## 3     3  2.18  2018
    ## 4     4  3.2   2018
    ## 5     5  9.27  2018
    ## 6     6  0.2   2018
    ## 7     7  2.39  2018

``` r
prcp_2017 = read_excel("./hw2data/HealthyHarborWaterWheelTotals2018-7-28.xlsx",
                       sheet = "2017 Precipitation", skip = 1 ) %>%  # import data 
  janitor::clean_names() %>%   # clean name
  na.omit() %>%  # remove na 
  mutate(year = 2017)  # put in year column 

prcp_2017
```

    ## # A tibble: 12 x 3
    ##    month total  year
    ##    <dbl> <dbl> <dbl>
    ##  1     1  2.34  2017
    ##  2     2  1.46  2017
    ##  3     3  3.57  2017
    ##  4     4  3.99  2017
    ##  5     5  5.64  2017
    ##  6     6  1.4   2017
    ##  7     7  7.09  2017
    ##  8     8  4.44  2017
    ##  9     9  1.95  2017
    ## 10    10  0     2017
    ## 11    11  0.11  2017
    ## 12    12  0.94  2017

``` r
combined_prcp = bind_rows(prcp_2018, prcp_2017)  # combine precipitation data 
    

month_df = data.frame(
  month = 1:12, name = month.name, stringsAsFactors = FALSE) # created month df
month_df
```

    ##    month      name
    ## 1      1   January
    ## 2      2  February
    ## 3      3     March
    ## 4      4     April
    ## 5      5       May
    ## 6      6      June
    ## 7      7      July
    ## 8      8    August
    ## 9      9 September
    ## 10    10   October
    ## 11    11  November
    ## 12    12  December

``` r
combined_prcp = inner_join(combined_prcp, month_df, by = "month") # added month df 

combined_prcp
```

    ## # A tibble: 19 x 4
    ##    month total  year name     
    ##    <dbl> <dbl> <dbl> <chr>    
    ##  1     1  0.96  2018 January  
    ##  2     2  5.3   2018 February 
    ##  3     3  2.18  2018 March    
    ##  4     4  3.2   2018 April    
    ##  5     5  9.27  2018 May      
    ##  6     6  0.2   2018 June     
    ##  7     7  2.39  2018 July     
    ##  8     1  2.34  2017 January  
    ##  9     2  1.46  2017 February 
    ## 10     3  3.57  2017 March    
    ## 11     4  3.99  2017 April    
    ## 12     5  5.64  2017 May      
    ## 13     6  1.4   2017 June     
    ## 14     7  7.09  2017 July     
    ## 15     8  4.44  2017 August   
    ## 16     9  1.95  2017 September
    ## 17    10  0     2017 October  
    ## 18    11  0.11  2017 November 
    ## 19    12  0.94  2017 December

``` r
combined_prcp %>%
  filter(year == 2018) %>%
  summarize(total = sum(total))
```

    ## # A tibble: 1 x 1
    ##   total
    ##   <dbl>
    ## 1  23.5

``` r
combined_prcp %>%
  filter(year == 2017) %>%
  summarize(total = sum(total))
```

    ## # A tibble: 1 x 1
    ##   total
    ##   <dbl>
    ## 1  32.9

``` r
wheel_data %>%
  filter(year == 2017) %>%
  summarize(median = median(sports_balls))
```

    ## # A tibble: 1 x 1
    ##   median
    ##    <int>
    ## 1      8

``` r
filter(wheel_data, year == 2018) %>%
  summarize(sum(weight_tons))
```

    ## # A tibble: 1 x 1
    ##   `sum(weight_tons)`
    ##                <dbl>
    ## 1               215.

There were 19 and 285 observations for the precipitation data set and the trash wheel dataset, respectively. The total precipitation for 2017 was 32.93 and 23.5 for 2018. The median for precipitation for 2017 and 2018 were 2.145 and 2.39, respectively. Some key variables for the trash dataset include the total trash weight in 2018 -- 215.36 tons, median number of sports balls in 2017 -- 8 balls, and the mean of plastic bottles thrown out in 2018 -- 1264.21875 bottles.

Problem 3
---------

``` r
devtools::install_github("p8105/p8105.datasets")
```

    ## Skipping install of 'p8105.datasets' from a github remote, the SHA1 (21f5ad1c) has not changed since last install.
    ##   Use `force = TRUE` to force installation

``` r
library(p8105.datasets)

data("brfss_smart2010") # import data set 


filtered_brfss = brfss_smart2010 %>%
  janitor::clean_names() %>% # clean names 
  filter(topic == "Overall Health") %>%
  select(-class, -topic, -question, -sample_size, -(confidence_limit_low:geo_location)) %>%
  spread(key = "response", value = "data_value") %>%  # making rows into columns
  janitor::clean_names() %>% # clean new columns 
  mutate(proportion_yv = excellent + very_good) # create proportion column

filtered_brfss  
```

    ## # A tibble: 2,125 x 9
    ##     year locationabbr locationdesc excellent  fair  good  poor very_good
    ##    <int> <chr>        <chr>            <dbl> <dbl> <dbl> <dbl>     <dbl>
    ##  1  2002 AK           AK - Anchor~      27.9   8.6  23.8   5.9      33.7
    ##  2  2002 AL           AL - Jeffer~      18.5  12.1  32.7   5.9      30.9
    ##  3  2002 AR           AR - Pulask~      24.1  12.5  29.9   4.2      29.3
    ##  4  2002 AZ           AZ - Marico~      21.6  10.3  26.9   4.6      36.6
    ##  5  2002 AZ           AZ - Pima C~      26.6   7.5  31.9   3.9      30.1
    ##  6  2002 CA           CA - Los An~      22.7  14.3  28.7   4.5      29.8
    ##  7  2002 CO           CO - Adams ~      21.2  14.4  29     4.2      31.2
    ##  8  2002 CO           CO - Arapah~      25.5   8    29.3   2.1      35.2
    ##  9  2002 CO           CO - Denver~      22.2  11.1  36.6   3        27.1
    ## 10  2002 CO           CO - Jeffer~      23.4  11.4  26.3   2.4      36.6
    ## # ... with 2,115 more rows, and 1 more variable: proportion_yv <dbl>

``` r
distinct(filtered_brfss, locationdesc) # distinct locations
```

    ## # A tibble: 404 x 1
    ##    locationdesc               
    ##    <chr>                      
    ##  1 AK - Anchorage Municipality
    ##  2 AL - Jefferson County      
    ##  3 AR - Pulaski County        
    ##  4 AZ - Maricopa County       
    ##  5 AZ - Pima County           
    ##  6 CA - Los Angeles County    
    ##  7 CO - Adams County          
    ##  8 CO - Arapahoe County       
    ##  9 CO - Denver County         
    ## 10 CO - Jefferson County      
    ## # ... with 394 more rows

``` r
# There are 404 distinct locations represented in this data set.
distinct(filtered_brfss, locationabbr) # distinct states
```

    ## # A tibble: 51 x 1
    ##    locationabbr
    ##    <chr>       
    ##  1 AK          
    ##  2 AL          
    ##  3 AR          
    ##  4 AZ          
    ##  5 CA          
    ##  6 CO          
    ##  7 CT          
    ##  8 DC          
    ##  9 DE          
    ## 10 FL          
    ## # ... with 41 more rows

``` r
# All states are represented in addition to D.C.

filtered_brfss %>%
  group_by(locationabbr) %>%
  summarize(n = n()) %>% # take count of number of state
  arrange(-n) # put the count in descending order to show which state was observed most 
```

    ## # A tibble: 51 x 2
    ##    locationabbr     n
    ##    <chr>        <int>
    ##  1 NJ             146
    ##  2 FL             122
    ##  3 NC             115
    ##  4 WA              97
    ##  5 MD              90
    ##  6 MA              79
    ##  7 TX              71
    ##  8 NY              65
    ##  9 SC              63
    ## 10 CO              59
    ## # ... with 41 more rows

``` r
# New Jersey was observed most with 146 counts. 


filtered_brfss %>%
  filter(year == 2002) %>% # filter by year 2002
  summarize(median = median(excellent, na.rm = TRUE)) # show median for excellent
```

    ## # A tibble: 1 x 1
    ##   median
    ##    <dbl>
    ## 1   23.6

``` r
filtered_brfss %>%
  filter(year == 2002) %>% # filter by year 
  ggplot(aes(x = excellent)) + geom_histogram() # create histogram
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 2 rows containing non-finite values (stat_bin).

![](hw2_files/figure-markdown_github/unnamed-chunk-4-1.png)

``` r
filtered_brfss %>%
  filter(locationdesc %in% 
           c("NY - New York County", "NY - Queens County")) %>% # filter by location
  ggplot(aes(x = year, y = proportion_yv, color = locationdesc)) +
  geom_point() # create scatter plot
```

![](hw2_files/figure-markdown_github/unnamed-chunk-4-2.png)
