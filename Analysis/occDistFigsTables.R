# create data aggregated by major and occupation
aggr <- df %>% filter(!is.na(loginc) & !is.na(major)) %>% 
               group_by(major,advdegree,occ1990) %>%
               dplyr::summarize(occ_shr = mean(occ_shr)) %>%
               ungroup %>%
               group_by(major,advdegree) %>%
               arrange(-occ_shr, .by_group=TRUE) %>%
               mutate(rank = row_number(),
                      occ_shr100 = 100*occ_shr,
                      occname = as.factor(occ1990),
                      occname = fct_recode(occname, "Legislators" = "3", "Chief executives and public administrators" = "4", "Financial managers" = "7", "Human resources and labor relations managers" = "8", "Managers and specialists in marketing, advertising, and public relations" = "13", "Managers in education and related fields" = "14", "Managers of medicine and health occupations" = "15", "Postmasters and mail superintendents" = "16", "Managers of food-serving and lodging establishments" = "17", "Managers of properties and real estate" = "18", "Funeral directors" = "19", "Managers of service organizations, n.e.c." = "21", "Managers and administrators, n.e.c." = "22", "Accountants and auditors" = "23", "Insurance underwriters" = "24", "Other financial specialists" = "25", "Management analysts" = "26", "Personnel, HR, training, and labor relations specialists" = "27", "Purchasing agents and buyers, of farm products" = "28", "Buyers, wholesale and retail trade" = "29", "Purchasing managers, agents and buyers, n.e.c." = "33", "Business and promotion agents" = "34", "Construction inspectors" = "35", "Inspectors and compliance officers, outside construction" = "36", "Management support occupations" = "37", "Architects" = "43", "Aerospace engineer" = "44", "Metallurgical and materials engineers, variously phrased" = "45", "Petroleum, mining, and geological engineers" = "47", "Chemical engineers" = "48", "Civil engineers" = "53", "Electrical engineer" = "55", "Industrial engineers" = "56", "Mechanical engineers" = "57", "Not-elsewhere-classified engineers" = "59", "Computer systems analysts and computer scientists" = "64", "Operations and systems researchers and analysts" = "65", "Actuaries" = "66", "Statisticians" = "67", "Mathematicians and mathematical scientists" = "68", "Physicists and astronomers" = "69", "Chemists" = "73", "Atmospheric and space scientists" = "74", "Geologists" = "75", "Physical scientists, n.e.c." = "76", "Agricultural and food scientists" = "77", "Biological scientists" = "78", "Foresters and conservation scientists" = "79", "Medical scientists" = "83", "Physicians" = "84", "Dentists" = "85", "Veterinarians" = "86", "Optometrists" = "87", "Podiatrists" = "88", "Other health and therapy" = "89", "Registered nurses" = "95", "Pharmacists" = "96", "Dietitians and nutritionists" = "97", "Respiratory therapists" = "98", "Occupational therapists" = "99"), 
                      occname = fct_recode(occname, "Physical therapists" = "103", "Speech therapists" = "104", "Therapists, n.e.c." = "105", "Physicians assistants" = "106", "Earth, environmental, and marine science instructors" = "113", "Biological science instructors" = "114", "Chemistry instructors" = "115", "Physics instructors" = "116", "Psychology instructors" = "118", "Economics instructors" = "119", "History instructors" = "123", "Sociology instructors" = "125", "Engineering instructors" = "127", "Math instructors" = "128", "Education instructors" = "139", "Law instructors" = "145", "Theology instructors" = "147", "Home economics instructors" = "149", "Humanities profs/instructors, college, nec" = "150", "Subject instructors (HS/college)" = "154", "Kindergarten and earlier school teachers" = "155", "Primary school teachers" = "156", "Secondary school teachers" = "157", "Special education teachers" = "158", "Teachers , n.e.c." = "159", "Vocational and educational counselors" = "163", "Librarians" = "164", "Archivists and curators" = "165", "Economists, market researchers, and survey researchers" = "166", "Psychologists" = "167", "Sociologists" = "168", "Social scientists, n.e.c." = "169", "Urban and regional planners" = "173", "Social workers" = "174", "Recreation workers" = "175", "Clergy and religious workers" = "176", "Lawyers " = "178", "Judges" = "179", "Writers and authors" = "183", "Technical writers" = "184", "Designers" = "185", "Musician or composer" = "186", "Actors, directors, producers" = "187", "Art makers: painters, sculptors, craft-artists, and print-makers" = "188", "Photographers" = "189", "Dancers" = "193", "Art/entertainment performers and related" = "194", "Editors and reporters" = "195", "Announcers" = "198", "Athletes, sports instructors, and officials" = "199", "Professionals, n.e.c." = "200", "Clinical laboratory technologies and technicians" = "203", "Dental hygenists" = "204", "Health record tech specialists" = "205", "Radiologic tech specialists" = "206", "Licensed practical nurses" = "207", "Health technologists and technicians, n.e.c." = "208", "Electrical and electronic (engineering) technicians" = "213", "Engineering technicians, n.e.c." = "214", "Mechanical engineering technicians" = "215", "Drafters" = "217", "Surveyors, cartographers, mapping scientists and technicians" = "218", "Biological technicians" = "223", "Chemical technicians" = "224", "Other science technicians" = "225", "Airplane pilots and navigators" = "226", "Air traffic controllers" = "227", "Broadcast equipment operators" = "228", "Computer software developers" = "229", "Programmers of numerically controlled machine tools" = "233", "Legal assistants, paralegals, legal support, etc" = "234", "Technicians, n.e.c." = "235", "Supervisors and proprietors of sales jobs" = "243", "Insurance sales occupations" = "253", "Real estate sales occupations" = "254"), 
                      occname = fct_recode(occname, "Financial services sales occupations" = "255", "Advertising and related sales jobs" = "256", "Sales engineers" = "258", "Salespersons, n.e.c." = "274", "Retail sales clerks" = "275", "Cashiers" = "276", "Door-to-door sales, street sales, and news vendors" = "277", "Sales demonstrators / promoters / models" = "283", "Office supervisors" = "303", "Computer and peripheral equipment operators" = "308", "Secretaries" = "313", "Stenographers" = "314", "Typists" = "315", "Interviewers, enumerators, and surveyors" = "316", "Hotel clerks" = "317", "Transportation ticket and reservation agents" = "318", "Receptionists" = "319", "Information clerks, nec" = "323", "Correspondence and order clerks" = "326", "Human resources clerks, except payroll and timekeeping" = "328", "Library assistants" = "329", "File clerks" = "335", "Records clerks" = "336", "Bookkeepers and accounting and auditing clerks" = "337", "Payroll and timekeeping clerks" = "338", "Cost and rate clerks (financial records processing)" = "343", "Billing clerks and related financial records processing" = "344", "Duplication machine operators / office machine operators" = "345", "Mail and paper handlers" = "346", "Office machine operators, n.e.c." = "347", "Telephone operators" = "348", "Other telecom operators" = "349", "Postal clerks, excluding mail carriers" = "354", "Mail carriers for postal service" = "355", "Mail clerks, outside of post office" = "356", "Messengers" = "357", "Dispatchers" = "359", "Inspectors, n.e.c." = "361", "Shipping and receiving clerks" = "364", "Stock and inventory clerks" = "365", "Meter readers" = "366", "Weighers, measurers, and checkers" = "368", "Material recording, scheduling, production, planning, and expediting clerks" = "373", "Insurance adjusters, examiners, and investigators" = "375", "Customer service reps, investigators and adjusters, except insurance" = "376", "Eligibility clerks for government programs; social welfare" = "377", "Bill and account collectors" = "378", "General office clerks" = "379", "Bank tellers" = "383", "Proofreaders" = "384", "Data entry keyers" = "385", "Statistical clerks" = "386", "Teachers aides" = "387", "Administrative support jobs, n.e.c." = "389", "Housekeepers, maids, butlers, stewards, and lodging quarters cleaners" = "405", "Private household cleaners and servants" = "407", "Supervisors of guards" = "415", "Fire fighting, prevention, and inspection" = "417", "Police, detectives, and private investigators" = "418", "Other law enforcement: sheriffs, bailiffs, correctional institution officers" = "423", "Crossing guards and bridge tenders" = "425", "Guards, watchmen, doorkeepers" = "426", "Protective services, n.e.c." = "427", "Bartenders" = "434", "Waiter/waitress" = "435", "Cooks, variously defined" = "436", "Food counter and fountain workers" = "438", "Kitchen workers" = "439", "Waiters assistant" = "443", "Misc food prep workers" = "444"), 
                      occname = fct_recode(occname, "Dental assistants" = "445", "Health aides, except nursing" = "446", "Nursing aides, orderlies, and attendants" = "447", "Supervisors of cleaning and building service" = "448", "Janitors" = "453", "Elevator operators" = "454", "Pest control occupations" = "455", "Supervisors of personal service jobs, n.e.c." = "456", "Barbers" = "457", "Hairdressers and cosmetologists" = "458", "Recreation facility attendants" = "459", "Guides" = "461", "Ushers" = "462", "Public transportation attendants and inspectors" = "463", "Baggage porters" = "464", "Welfare service aides" = "465", "Child care workers" = "468", "Personal service occupations, nec" = "469", "Farmers (owners and tenants)" = "473", "Horticultural specialty farmers" = "474", "Farm managers, except for horticultural farms" = "475", "Managers of horticultural specialty farms" = "476", "Farm workers" = "479", "Marine life cultivation workers" = "483", "Nursery farming workers" = "484", "Supervisors of agricultural occupations" = "485", "Gardeners and groundskeepers" = "486", "Animal caretakers except on farms" = "487", "Graders and sorters of agricultural products" = "488", "Inspectors of agricultural products" = "489", "Timber, logging, and forestry workers" = "496", "Fishers, hunters, and kindred" = "498", "Supervisors of mechanics and repairers" = "503", "Automobile mechanics" = "505", "Bus, truck, and stationary engine mechanics" = "507", "Aircraft mechanics" = "508", "Small engine repairers" = "509", "Auto body repairers" = "514", "Heavy equipment and farm equipment mechanics" = "516", "Industrial machinery repairers" = "518", "Machinery maintenance occupations" = "519", "Repairers of industrial electrical equipment " = "523", "Repairers of data processing equipment" = "525", "Repairers of household appliances and power tools" = "526", "Telecom and line installers and repairers" = "527", "Repairers of electrical equipment, n.e.c." = "533", "Heating, air conditioning, and refigeration mechanics" = "534", "Precision makers, repairers, and smiths" = "535", "Locksmiths and safe repairers" = "536", "Office machine repairers and mechanics" = "538", "Repairers of mechanical controls and valves" = "539", "Elevator installers and repairers" = "543", "Millwrights" = "544", "Mechanics and repairers, n.e.c." = "549", "Supervisors of construction work" = "558", "Masons, tilers, and carpet installers" = "563", "Carpenters" = "567", "Drywall installers" = "573", "Electricians" = "575", "Electric power installers and repairers" = "577", "Painters, construction and maintenance" = "579", "Paperhangers" = "583", "Plasterers" = "584", "Plumbers, pipe fitters, and steamfitters" = "585", "Concrete and cement workers" = "588", "Glaziers" = "589", "Insulation workers" = "593", "Paving, surfacing, and tamping equipment operators" = "594", "Roofers and slaters" = "595", "Sheet metal duct installers" = "596", "Structural metal workers" = "597"), 
                      occname = fct_recode(occname, "Drillers of earth" = "598", "Construction trades, n.e.c." = "599", "Drillers of oil wells" = "614", "Explosives workers" = "615", "Miners" = "616", "Other mining occupations" = "617", "Production supervisors or foremen" = "628", "Tool and die makers and die setters" = "634", "Machinists" = "637", "Boilermakers" = "643", "Precision grinders and filers" = "644", "Patternmakers and model makers" = "645", "Lay-out workers" = "646", "Engravers" = "649", "Tinsmiths, coppersmiths, and sheet metal workers" = "653", "Cabinetmakers and bench carpenters" = "657", "Furniture and wood finishers" = "658", "Other precision woodworkers" = "659", "Dressmakers and seamstresses" = "666", "Tailors" = "667", "Upholsterers" = "668", "Shoe repairers" = "669", "Other precision apparel and fabric workers" = "674", "Hand molders and shapers, except jewelers " = "675", "Optical goods workers" = "677", "Dental laboratory and medical appliance technicians" = "678", "Bookbinders" = "679", "Other precision and craft workers" = "684", "Butchers and meat cutters" = "686", "Bakers" = "687", "Batch food makers" = "688", "Adjusters and calibrators" = "693", "Water and sewage treatment plant operators" = "694", "Power plant operators" = "695", "Plant and system operators, stationary engineers " = "696", "Other plant and system operators" = "699", "Lathe, milling, and turning machine operatives" = "703", "Punching and stamping press operatives" = "706", "Rollers, roll hands, and finishers of metal" = "707", "Drilling and boring machine operators" = "708", "Grinding, abrading, buffing, and polishing workers" = "709", "Forge and hammer operators" = "713", "Fabricating machine operators, n.e.c." = "717", "Molders, and casting machine operators" = "719", "Metal platers" = "723", "Heat treating equipment operators" = "724", "Wood lathe, routing, and planing machine operators" = "726", "Sawing machine operators and sawyers" = "727", "Shaping and joining machine operator (woodworking)" = "728", "Nail and tacking machine operators (woodworking)" = "729", "Other woodworking machine operators" = "733", "Printing machine operators, n.e.c." = "734", "Photoengravers and lithographers" = "735", "Typesetters and compositors" = "736", "Winding and twisting textile/apparel operatives" = "738", "Knitters, loopers, and toppers textile operatives" = "739", "Textile cutting machine operators" = "743", "Textile sewing machine operators" = "744", "Shoemaking machine operators" = "745", "Pressing machine operators (clothing)" = "747", "Laundry workers" = "748", "Misc textile machine operators" = "749", "Cementing and gluing maching operators" = "753", "Packers, fillers, and wrappers" = "754", "Extruding and forming machine operators" = "755", "Mixing and blending machine operatives" = "756", "Separating, filtering, and clarifying machine operators" = "757", "Painting machine operators" = "759", "Roasting and baking machine operators (food)" = "763"), 
                      occname = fct_recode(occname, "Washing, cleaning, and pickling machine operators" = "764", "Paper folding machine operators" = "765", "Furnace, kiln, and oven operators, apart from food" = "766", "Crushing and grinding machine operators" = "768", "Slicing and cutting machine operators" = "769", "Motion picture projectionists" = "773", "Photographic process workers" = "774", "Machine operators, n.e.c." = "779", "Welders and metal cutters" = "783", "Solderers" = "784", "Assemblers of electrical equipment" = "785", "Hand painting, coating, and decorating occupations" = "789", "Production checkers and inspectors" = "796", "Graders and sorters in manufacturing" = "799", "Supervisors of motor vehicle transportation" = "803", "Truck, delivery, and tractor drivers" = "804", "Bus drivers" = "808", "Taxi cab drivers and chauffeurs" = "809", "Parking lot attendants" = "813", "Railroad conductors and yardmasters" = "823", "Locomotive operators (engineers and firemen)" = "824", "Railroad brake, coupler, and switch operators" = "825", "Ship crews and marine engineers" = "829", "Water transport infrastructure tenders and crossing guards" = "834", "Operating engineers of construction equipment" = "844", "Crane, derrick, winch, and hoist operators" = "848", "Excavating and loading machine operators" = "853", "Misc material moving occupations" = "859", "Helpers, constructions" = "865", "Helpers, surveyors" = "866", "Construction laborers" = "869", "Production helpers" = "874", "Garbage and recyclable material collectors" = "875", "Materials movers: stevedores and longshore workers" = "876", "Stock handlers" = "877", "Machine feeders and offbearers" = "878", "Freight, stock, and materials handlers" = "883", "Garage and service station related occupations" = "885", "Vehicle washers and equipment cleaners" = "887", "Packers and packagers by hand" = "888", "Laborers outside construction" = "889", "Military" = "905", "Unemployed" = "991", "Unknown" = "999")
                     )
aggr2 <- aggr %>% ungroup %>%
                  mutate(majorAgg = dplyr::recode(
                                                  major,
                                                        `21` = 2L,
                                                        `26` = 10L,
                                                        `3`  = 5L,
                                                        `23` = 1L,
                                                        `37` = 6L,
                                                        `43` = 1L,
                                                        `14` = 10L,
                                                        `25` = 2L,
                                                        `9`  = 5L,
                                                        `6`  = 10L,
                                                        `8`  = 5L,
                                                        `28` = 1L,
                                                        `34` = 3L,
                                                        `7`  = 7L,
                                                        `13` = 7L,
                                                        `11` = 10L,
                                                        `16` = 2L,
                                                        `5`  = 5L,
                                                        `10` = 5L,
                                                        `18` = 10L,
                                                        `27` = 11L,
                                                        `50` = 1L,
                                                        `17` = 2L,
                                                        `32` = 8L,
                                                        `30` = 6L,
                                                        `41` = 6L,
                                                        `42` = 11L,
                                                        `47` = 3L,
                                                        `33` = 12L,
                                                        `38` = 6L,
                                                        `24` = 4L,
                                                        `22` = 2L,
                                                        `1`  = 7L,
                                                        `4`  = 5L,
                                                        `48` = 8L,
                                                        `19` = 2L,
                                                        `20` = 10L,
                                                        `31` = 1L,
                                                        `51` = 8L,
                                                        `35` = 8L,
                                                        `36` = 11L,
                                                        `46` = 11L,
                                                        `2`  = 10L,
                                                        `39` = 11L,
                                                        `15` = 12L,
                                                        `40` = 9L,
                                                        `29` = 11L,
                                                        `44` = 9L,
                                                        `49` = 9L,
                                                        `12` = 4L,
                                                        `45` = 11L
                                                 ),
                         majorAggAgg = case_when(
                                                 (majorAgg %in% c(4))          ~ 1, # "Education Major"
                                                 (majorAgg %in% c(11))         ~ 2, # "Social Sciences Major"
                                                 (majorAgg %in% c(1,3,6,9,12)) ~ 3, # "Other Major"
                                                 (majorAgg %in% c(2))          ~ 4, # "Business Major"
                                                 (majorAgg %in% c(5,7,8,10))   ~ 5, # "STEM Major"
                                                 TRUE                          ~ 3  #remaining cases are classified as "Other Major"
                                                )
                        ) %>% 
                  select(-majorAgg) %>%
                  rename(majorAgg = majorAggAgg) %>%
                  mutate(majorAgg = as.factor(majorAgg),
                         majorAgg = fct_recode(majorAgg, "Edu." = "1", "Soc. Sci." = "2", "Other" = "3", "Bus." = "4", "STEM" = "5")
                        )

aggr2 %<>% mutate(related = case_when(occ_shr>0.02 ~ "$\\checkmark$", occ_shr<=0.02 ~ "")) %>%
           select(-major,-occ_shr)

aggr3 <- aggr2 %>%
         distinct(occ1990,occname,majorAgg,advdegree,related) %>%
         mutate(related = as.factor(related))

aggr4 <- aggr3 %>% 
         filter(related!="") %>% 
         pivot_wider(names_from = majorAgg, values_from = related) %>% 
         replace_na(list( `Edu.`="", `Soc. Sci.`="", Other = "", `Bus.` = "", STEM = "") )

# Function to create plots
plotter <- function(df,maj,adv) {
    maxrank <- df %>% filter(major==maj & advdegree==adv & occ_shr100>2) %>% `$`(rank) %>% max
    p <- ggplot( df %>% filter(major==maj & advdegree==adv) ,
                  aes(rank,occ_shr100) 
               ) + 
         geom_col(fill = "#1A476F") + 
         geom_vline(xintercept=maxrank+.5, color="maroon") + 
         labs(y = "Share (%)", x = "Occupation Rank") + theme_minimal() +
         theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_line(size = 0.05), panel.grid.major.x = element_line(size = 0.05), axis.text.y = element_text(color = "black"), axis.text.x = element_text(color = "black"), axis.title.x = element_text(color = "black"), axis.title.y = element_text(color = "black"), panel.border = element_rect(color="black", fill=NA)) +
         scale_y_continuous(limits = c(0,50), breaks = seq(0,50,by=10))
    ggsave(paste0("../Paper/Graphics/relOccBarM",maj,"A",as.numeric(adv),".eps"), device = "eps", width=5, height = 4, units = "in")
}

# Functions to create tables
tabler <- function(df,maj,adv) {
    maxrank <- df %>% filter(major==maj & advdegree==adv & occ_shr100>2) %>% `$`(rank) %>% max
    temp <- df %>% ungroup %>% filter(major==maj & advdegree==adv & rank<=maxrank+3) %>% select(occname,occ_shr100) %>% rename(Occupation = occname, `Share (%)`=occ_shr100)
    datasummary_df(temp, output=paste0("../Paper/Tables/relOccDetListM",maj,"A",as.numeric(adv),".tex"))
    # remove LaTeX table headers
    system(paste0("sed -i 's/\\\\begin{table}\\[H\\]//' ../Paper/Tables/relOccDetListM",maj,"A",as.numeric(adv),".tex"))
    system(paste0("sed -i 's/\\\\end{table}//' ../Paper/Tables/relOccDetListM",maj,"A",as.numeric(adv),".tex"))
    system(paste0("sed -i 's/\\\\centering//' ../Paper/Tables/relOccDetListM",maj,"A",as.numeric(adv),".tex"))
}
tabler2 <- function(df,adv) {
    temp <- df %>% ungroup %>% filter(advdegree==adv) %>% arrange(occ1990) %>% select(occname,`Edu.`,`Soc. Sci.`,Other,`Bus.`,STEM) %>% rename(Occupation = occname)
    datasummary_df(temp, output=paste0("../Paper/Tables/relOccDetListMA",as.numeric(adv),".tex"), notes="Note: Occupations not related to any college major are excluded from this table.")
    # remove LaTeX table headers
    system(paste0("sed -i 's/\\\\begin{table}\\[H\\]//' ../Paper/Tables/relOccDetListMA",as.numeric(adv),".tex"))
    system(paste0("sed -i 's/\\\\end{table}//' ../Paper/Tables/relOccDetListMA",as.numeric(adv),".tex"))
    system(paste0("sed -i 's/\\\\centering//' ../Paper/Tables/relOccDetListMA",as.numeric(adv),".tex"))
    for (ggg in seq(1,10)) {
        system(paste0("sed -i 's/textbackslash{}//' ../Paper/Tables/relOccDetListMA",as.numeric(adv),".tex"))
    }
    for (ggg in seq(1,20)) {
        system(paste0("sed -i 's/\\\\\\$/\\$/' ../Paper/Tables/relOccDetListMA",as.numeric(adv),".tex"))
        system(paste0("sed -i 's/checkmark\\\\\\$/checkmark\\$/' ../Paper/Tables/relOccDetListMA",as.numeric(adv),".tex"))
    }
}

# call the functions for each major/adv degree combo
for (a in c(TRUE,FALSE)) {
    tabler2(aggr4,a)
    for (m in c(13,16,24,41)) {
        plotter(aggr,m,a)
        tabler(aggr,m,a)
    }
}

