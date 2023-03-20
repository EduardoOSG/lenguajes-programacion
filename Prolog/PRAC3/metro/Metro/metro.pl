% Lineas del Metro

linea(1,[observatorio,tacubaya,juanacatlan,chapultepec,sevilla,cuahtemoc,
	 balderas,salto_del_agua,isabel_la_catolica,pino_suarez,merced,
	 candelaria,san_lazaro,moctezuma,balbuena,bulevard_puerto_aereo,
	 gomez_farias,zaragoza,pantitlan]).
linea(2,[cuatro_caminos,panteones,tacuba,cuitlahuac,popotla,colegio_militar,
	 normal,san_cosme,revolucion,hidalgo,bellas_artes,allende,zocalo,
	 pino_suarez,san_antonio_abad,chabacano,viaducto,xola,villa_de_cortes,
	 nativitas,portales,ermita,general_anaya,tasquena]).
linea(3,[indios_verdes,deportivo_18_de_marzo,potrero,la_raza,tlatelolco,
	 guerrero,hidalgo,juarez,balderas,nignos_heroes,hospital_general,
	 centro_medico,etiopia,eugenia,division_del_norte,zapata,coyoacan,
	 viveros,miguel_angel_de_quevedo,copilco,universidad]).
linea(4,[martin_carrera,talisman,bondojito,consulado,canal_del_norte,
	 morelos,candelaria,fray_servando,jamaica,santa_anita]).
linea(5,[politecnico,instituto_del_petroleo,autobuses_norte,la_raza,misterios,
	 valle_gomez,consulado,eduardo_molina,aragon,oceania,terminal_aerea,
	 hangares,pantitlan]).
linea(6,[el_rosario,tezozomoc,azcapotzalco,ferreria,norte_45,vallejo,
	 instituto_del_petroleo,lindavista,deportivo_18_de_marzo,basilica,
	 martin_carrera]).
linea(7,[el_rosario,aquiles_serdan,camarones,refineria,tacuba,
	 rio_san_joaquin,polanco,auditorio,constituyentes,tacubaya,
	 san_pedro_de_los_pinos,san_antonio,mixcoac,barranca_del_muerto]).
linea(8,[garibaldi,bellas_artes,san_juan_de_letran,salto_del_agua,
	 doctores,obrera,chabacano,la_viga,santa_anita,coyuya,iztacalco,
	 apatlaco,aculco,escuadron_201,atlalilco,iztapalapa,
	 cerro_de_la_estrella,uami,constitucion_de_1917]).
linea(9,[tacubaya,patriotismo,chilpancingo,centro_medico,lazaro_cardenas,
	 chabacano,jamaica,mixihuca,velodromo,ciudad_deportiva,puebla,
	 pantitlan]).
linea(a,[pantitlan, agricola_oriental,canal_de_san_juan,tepalcates,guelatao,
	 pegnon_viejo,acatitla,santa_marta,los_reyes,la_paz]).
linea(b,[ciudad_azteca,plaza_aragon,olimpica,ecatepec,muzquiz,
	 rio_de_los_remedios,impulsora,nezahualcoyotl,villa_de_aragon,
	 bosque_de_aragon,deportivo_oceania,oceania,romero_rubio,
	 r_flores_magon,san_lazaro,morelos,tepito,lagunilla,garibaldi,
	 guerrero,buenavista]).
linea(12,[mixcoac,insurgentes_sur,hospital_20_de_noviembre,zapata,
	  parque_de_los_venados,eje_central,ermita,mexicaltzingo,
	  atlalico,culhuacan,san_andres_tomatlan,lomas_estrella,calle_11,
	  periferico_oriente,tezonco,olivos,nopalera,zapotitlan,
	  tlaltenco,tlahuac]).

% Utilidades

member(X,[X|Xs]).
member(X,[Y|Xs]):-member(X,Xs).

append([],X,X).
append([X|Xs],Ys,[X|Zs]):-append(Xs,Ys,Zs).
