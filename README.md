-- CODE DESCRIPTION --

The goal of the game is to travel the world, collect souvenirs from each destination, and bring them back home.

There are several available actions. By typing "look", the player can see the surroundings. By typing "inventory", the player can view their current inventory. Items in the current room can be taken by typing "take" followed by the name of the item. Items in the player's inventory can be dropped by typing "drop" followed by the name of the item. The player can move through available exits by typing "move" followed by one of the cardinal directions.

The newest feature is having multiple maps that the player can move between, triggered by holding certain items in the inventory. The main change I made to implement the multiple maps was creating more Directions. I added an Exit to each room that transports the player to a new map, with a unique Direction for each transportation. These new exits are accounted for in the same move function, but they are not able to be moved through normally with the command line. Instead, they are triggered by the presence of certain items in the player's inventory, which automatically calls the move function when the player is in the room and the conditions are satisfied.

One other change that was necessitated by this implementation was the move messages. Before, every movement would set the message to "You go ____", where the blank is the show instance of thd direction. However, I wanted more specific move messages for transporting between maps to set them apart from normal movement, so I created the moveMessage function in GameState.hs that maps a direction to a specific message.

Another new change was the NPC implementation. I created an NPC module in NPC.hs defining the NPC datatype, which contains a name, description, hint, location, and an Int called looks which tracks how many times the NPC has been in the room with the look command being called. The NPC will say its hint if looks is equal to 0, otherwise only the description will be read. The one NPC in the game is also defined in this module.



-- WALKTHROUGH --

The game starts in Hyde Park, Chicago, with the player starting in the bedroom of their house. The first objective is to acquire a plane ticket. To do this, the player must find a CS professor and give the professor their homework. In exchange, the professor will drop a plane ticket to London. The professor starts at Crerar Quad but moves randomly every time a conjunction is entered. To get to the CS professor in one conjunction as to prevent any movement, type the following:

south and south and south and south and south and south and south and south and west and west and west and west and south

Then, type the following:

drop homework
take london plane ticket

Once the plane ticket has been acquired, the player must make their way back home to a taxi sitting next to the front yard of the house. Upon entering the room with the taxi, the player will be transported to the loading zone at the airport if they are holding the London plane ticket. To get to the taxi from Crerar Quad, type the following:

north and east and east and east and east and north and north and north and north and north and west

Once at the airport, the player must make their way to the plane, where they will be transported to the London airport if they are holding the London plane ticket. Upon being transported, the game also removes the London plane ticket from the player's inventory. To get to the plane from the loading zone, type the following:

north and north and north and east and east and east

The plane takes the player to the gate at the London airport. The player must make their way to downtown London by navigating through the airport to the tube station. There is a tube ticket in the station, which the player must take and move onto the train, which will transport the player downtown if they are holding the tube ticket. Upon being transported, the game also removes the tube ticket from the player's inventory. To get downtown from the gate, type the following:

east and east and east and east
take westminster tube ticket
east

The player is now in the Westminster tube station in downtown London. The objective of this part is to collect a souvenir, along with a return tube ticket to the airport and a plane ticket to Amsterdam. The souvenir is at the Sky Garden and it can be obtained by typing the following:

east and east and east and north
take tea set

From the Sky Garden, the player must go to the Royal Botanic Gardens to collect the return tube ticket. It can be obtained by typing the following:

west and west and west and south and west and south
take heathrow airport tube ticket

From the Royal Botanic Gardens, the player must go to Madame Tussauds to collect the plane ticket. It can be obtained by typing the following:

east and north and north and west
take amsterdam plane ticket

The player must then return to the Westminster tube station, which will transport them back to the airport tube station if the tea set, return tube ticket, and plane ticket are all in the player's inventory. Upon being transported, the game also removes the return tube ticket from the player's inventory. To get back to the airport from Madame Tussauds, type the following:

east and south

Once at the airport, the player must make their way to the plane, where they will be transported to the Amsterdam airport if they are holding the Amsterdam plane ticket. Upon being transported, the game also removes the Amsterdam plane ticket from the player's inventory. To get to the plane from the airport tube station, type the following:

west and west and west and west and west

The plane takes the player to the gate at the Amsterdam airport. This airport works very similarly to the London airport, where the player must collect a train ticket from the train station before boarding the train to get downtown. Upon being transported, the game also removes the train ticket from the player's inventory. To get downtown from the gate, type the following:

south and south and south and south
take centraal train ticket
south

The player is now in Centraal station in downtown Amsterdam. The objective of this part is to collect a souvenir, along with a return train ticket to the airport and a plane ticket to Paris. The souvenir is at the Flower Market and it can be obtained by typing the following:

south and south and west and south
take stroopwafel

From the Flower Market, the player must go to Oosterpark to collect the return train ticket. It can be obtained by typing the following:

north and east and east and east
take schiphol airport train ticket

From Oosterpark, the player must go to Vondelpark to collect the plane ticket. It can be obtained by typing the following:

west and west and west and south and west and west
take paris plane ticket

The player must then return to Centraal station, which will transport them back to the airport train station if the stroopwafel, return train ticket, and plane ticket are all in the player's inventory. Upon being transported, the game also removes the return train ticket from the player's inventory. To get back to the airport from Vondelpark, type the following:

east and east and north and north and north and east

Once at the airport, the player must make their way to the plane, where they will be transported to the Paris airport if they are holding the Paris plane ticket. Upon being transported, the game also removes the Paris plane ticket from the player's inventory. To get to the plane from the airport train station, type the following:

north and north and north and north and north

The plane takes the player to the gate at the Paris airport. This airport works very similarly to the other airports, where the player must collect a train ticket from the train station before boarding the train to get downtown. Upon being transported, the game also removes the train ticket from the player's inventory. To get downtown from the gate, type the following:

west and west and west and west
take gare du nord train ticket
west

The player is now in Gare du Nord in downtown Paris. The objective of this part is to collect a souvenir, along with a return train ticket to the airport and a plane ticket to Prague. The souvenir is at the Moulin Rouge and it can be obtained by typing the following:

west and north
take croissant

From the Moulin Rouge, the player must go to Place des Vosges to collect the return train ticket. It can be obtained by typing the following:

south and south and east and east and south and east and east
take charles de gaulle airport train ticket

From Place des Vosges, the player must go to the catacombs to collect the plane ticket. It can be obtained by typing the following:

west and west and north and west and west and south and south and south
take prague plane ticket

The player must then return to Gare du Nord, which will transport them back to the airport train station if the croissant, return train ticket, and plane ticket are all in the player's inventory. Upon being transported, the game also removes the return train ticket from the player's inventory. To get back to the airport from the catacombs, type the following:

north and north and north and east and north

Once at the airport, the player must make their way to the plane, where they will be transported to the Prague airport if they are holding the Prague plane ticket. Upon being transported, the game also removes the Prague plane ticket from the player's inventory. To get to the plane from the airport train station, type the following:

east and east and east and east and east

The plane takes the player to the gate at the Prague airport. This airport works very similarly to the other airports, where the player must collect a train ticket from the train station before boarding the train to get downtown. Upon being transported, the game also removes the train ticket from the player's inventory. To get downtown from the gate, type the following:

south and south and south and south
take main station train ticket
south

The player is now in Main station in downtown Prague. The objective of this part is to collect a souvenir, along with a return train ticket to the airport and a plane ticket to Istanbul. The souvenir is at the Old Jewish Cemetary and it can be obtained by typing the following:

west and north and west
take red garnet

From the Old Jewish Cemetary, the player must go to Dancing House to collect the return train ticket. It can be obtained by typing the following:

south and south and south and south
take vaclav havel airport train ticket

From Dancing House, the player must go to Strahov Monastery to collect the plane ticket. It can be obtained by typing the following:

north and north and west and west and west and south and west
take istanbul plane ticket

The player must then return to Main station, which will transport them back to the airport train station if the red garnet, return train ticket, and plane ticket are all in the player's inventory. Upon being transported, the game also removes the return train ticket from the player's inventory. To get back to the airport from Strahov Monastery, type the following:

east and east and north and east and east and north and east and east

Once at the airport, the player must make their way to the plane, where they will be transported to the Istanbul airport if they are holding the Istanbul plane ticket. Upon being transported, the game also removes the Istanbul plane ticket from the player's inventory. To get to the plane from the airport train station, type the following:

north and north and north and north and north

The plane takes the player to the gate at the Istanbul airport. This airport works similarly to the other airports, but there is a taxi instead of a train station, so the player will be transported downtown automatically upon reaching the taxi. To get downtown from the gate, type the following:

north and north and north and north and north

The player is now in front of a taxi in downtown Istanbul. The objective of this part is to collect a souvenir, along with a plane ticket to Chicago. The souvenir is at the Grand Bazaar and it can be obtained by typing the following:

east
take turkish delight

From the Grand Bazaar, the player must go to Boğaziçi University to collect the plane ticket. It can be obtained by typing the following:

south and east and east and north and north and north
take chicago plane ticket

The player must then return to the taxi, which will transport them back to the airport loading zone if the Turkish Delight and plane ticket are both in the player's inventory. To get back to the airport from Boğaziçi University, type the following:

south and south and south and west and west and north and west

Once at the airport, the player must make their way to the plane, where they will be transported back to the Chicago airport if they are holding the Chicago plane ticket. Upon being transported, the game also removes the Chicago plane ticket from the player's inventory. To get to the plane from the airport loading zone, type the following:

south and south and south and south and south

The plane takes the player back to the gate at the Chicago airport. The player must then return to Hyde Park by navigating to the airport loading zone, where they will be transported back to the front yard of the house if the Turkish Delight is in the player's inventory. Even though the game only checks for the Turkish Delight at this stage, all five souvenirs are necessary to win the game. To get back to Hyde Park from the gate, type the following:

west and west and south and south and south

From the front yard, all that's left to do is to go back to the bedroom with all five souvenirs. To get to the bedroom, type the following:

north and north and north

Congratulations, you've won the game!