// version control
// Date  | Change
// 4/2	 | 
var moose = new BiteAgent({
	name: "Moose", description: "Browser", 
	cellSize: 100, //100 meters
	lifecycle: new BiteLifeCycle({ voltinism: 1, // max 1 gen. per year, growth rate adjusts true population growth
				dieAfterDispersal: false, // cell does not die even with dispersal
				spreadFilter: 'agentBiomass>0', // condition that needs to be met before spread 
				spreadDelay: 1, // number of years after colonization
				spreadInterval: 1   // min. frequency of spread (1/years)
		}),
				
		
    dispersal: new BiteDistribution({
		}), // agent is distributed evenly across the landscape 
    
	
	colonization: new BiteColonization({ 
		dispersalFilter: 'dispersalGrid', // 100% chance of new colonization (no dispersal filter / limits)
		speciesFilter: 'species=Bene', // filter by host species
		saplingFilter:'height<3', // saplings below 3 meters are the host
		initialAgentBiomass: 0.95*465 // initial agent biomass in each cell calculated assuming moose density to be 0.95 animals per 1 km2 (152 per 1000) * average mass of 465 kg (estimated) per animal  
	   }),

	
growth: new BiteBiomass({
		hostTrees: '(species=Bene or species=Potr) and height<3', // these trees are the hosts
		hostBiomass: function(cell) {
		   cell.reloadSaplings(); // reload the sapling list // filter by birch and aspen
		   var bm_Bene = cell.saplings.sum('nrep*foliagemass', 'species = Bene and height<3');
		   var bm_Potr = cell.saplings.sum('nrep*foliagemass', 'species = Potr and height<3');
		   var bm_saplings = bm_Bene + bm_Potr; // calculate available total biomass for saplings = foliage biomass * number of represented stems per cohort
		   return bm_saplings; // total sum of host biomass in the cell
	    },
		mortality: function(cell) {
			 var biom = cell.value('agentBiomass');
			 if (Globals.year>2 & biom<1){  
                return 1;
                }
				else {
                return 0;
		} }, //registering cells where biomass has dropped too low as dead so they can be recolonized
		growthFunction: 'M + r*t', // where K=hostbiomass / consumption; M=agentBiomass; r=relative growth rate coefficient; t=time
		growthRateFunction: '0',  // no population growth, assume that the local population is in equilibrium (birth=death,immigrate=emigrate)
		consumption: 13.8, //yearly consumption kg / body weight kg
		// 32 kg daily * 65% tree biomass * 365 = 7592 kg per year / 465 kg = 13.804
		growthIterations: 10,  //10 iterative rounds of biomass calculation in case the biomass in the cell runs out during the time step
		}),		
		
	impact: new BiteImpact({ // species preferences
		impactFilter: 'agentImpact>0', // impact occurs once the agent has consumed the first biomass units of the host
		impact: [	// impact arrays
		
			// GROWTH IMPACTS
			{target: 'browsing', treeFilter: 'species = Bene', // impacts birch sapling growth
			fractionOfTrees: function(cell, agentImpact, hostBiomass) { 
						cell.reloadSaplings(); // reload the sapling list
						cell.saplings.filter('species = Bene'); 
						var bm_Bene = cell.saplings.sum('nrep*foliagemass'); 
						return bm_Bene/hostBiomass*0.30*agentImpact/hostBiomass; 
					}},
					
			{target: 'browsing', treeFilter: 'species = Potr', // impacts aspen sapling growth
			 fractionOfTrees: function(cell, agentImpact, hostBiomass) { 
						cell.reloadSaplings(); // reload the sapling list
						cell.saplings.filter('species = Potr'); 
						var bm_Potr = cell.saplings.sum('nrep*foliagemass'); 
						return bm_Potr/hostBiomass*0.70*agentImpact/hostBiomass;
					}},	
					
			// MORTALITY IMPACTS		
			{target: 'sapling', treeFilter: 'species = Bene', // impacts birch sapling mortality
			fractionOfTrees: function(cell, agentImpact, hostBiomass) { 
						cell.reloadSaplings(); // reload the sapling list
						cell.saplings.filter('species = Bene'); 
						var bm_Bene = cell.saplings.sum('nrep*foliagemass'); 
						return bm_Bene/hostBiomass*0.30*agentImpact/hostBiomass; 
					}},
			
			{target: 'sapling', treeFilter: 'species = Potr', // impacts aspen sapling mortality
			 fractionOfTrees: function(cell, agentImpact, hostBiomass) { 
						cell.reloadSaplings(); // reload the sapling list
						cell.saplings.filter('species = Potr'); 
						var bm_Potr = cell.saplings.sum('nrep*foliagemass'); 
						return bm_Potr/hostBiomass*0.70*agentImpact/hostBiomass;
					}},	
		]
	}),
	
	output: new BiteOutput({
		outputFilter: "active=true",
		tableName: 'BiteTabMoose',
		columns: ['yearsLiving', 'hostBiomass', 'agentImpact','agentBiomass']
		
	}),
	onYearEnd: function(agent) { agent.saveGrid('yearsLiving', 'temp/moose_base.asc'); }

});

//function randomSpread(n, gr) {
//    for (var i=0;i<n;++i) {
 //       var x = Math.random()*gr.width;
 //   var y = Math.random()*gr.height;
  //  gr.setValue(x,y,1); // x coordinates, y coordinates, number of moose - putting in x and y does random coordinates
  //  }
//}

moose.verbose = true;