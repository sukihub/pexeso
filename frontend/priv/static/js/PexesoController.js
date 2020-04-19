'use strict';

pexeso.controller('PexesoController', function PexesoController($scope, $routeParams) {

    var TIMEOUT = 1000;
    var timeouts;

    $scope.game_name = $routeParams.game_name;
    $scope.nickname = '';
	$scope.stats = {};
    $scope.playing = false;

    $scope.join = function()
    {
    	var pexeso = new Pexeso($routeParams.game_name, $scope.nickname, function()
        {
            $scope.playing = true;
            $scope.$apply();
        });
    	
    	pexeso.onstatsupdate = function(stats)
    	{
    		$scope.stats = stats;
            console.log(stats);
            $scope.$apply();
    	};

        pexeso.onplaygroundupdate = function(playground)
        {
            var max = playground[playground.length - 1];

            var draw = new Array(max);
            timeouts = new Array(max);
            for (var i = 0; i < playground.length; i++) draw[playground[i] - 1] = playground[i];

            $scope.playground = draw;
            $scope.$apply();
        };

        pexeso.onturn = function(player, card_id, content)
        {
            var i = card_id - 1;

            $scope.playground[i] = content;
            $scope.stats[player].turns += 1;

            clearTimeout(timeouts[i]);
            
            if (player == $scope.nickname)
            {
                timeouts[i] = setTimeout(function()
                {
                    pexeso.turnCard(card_id);
                }, 
                3000);
            }

            $scope.$apply();
        };

        pexeso.onclose = function(player, card_id, content)
        {
            var i = card_id - 1;

            $scope.playground[i] = card_id;
            clearTimeout(timeouts[i]);
            
            $scope.$apply();
        };

        pexeso.onfail = function(player, card_id_1, content_1, card_id_2, content_2)
        {
            var i = card_id_1 - 1, j = card_id_2 - 1;

            $scope.playground[i] = content_1;
            $scope.playground[j] = content_2;
            $scope.stats[player].turns += 1;

            clearTimeout(timeouts[i]);
            clearTimeout(timeouts[j]);

            timeouts[i] = setTimeout(function() 
            {
                $scope.playground[i] = card_id_1;
                $scope.$apply();
            }, 
            TIMEOUT);

            timeouts[j] = setTimeout(function() 
            {
                $scope.playground[j] = card_id_2;
                $scope.$apply();
            }, 
            TIMEOUT);

            $scope.$apply();
        };

        pexeso.onpick = function(player, card_id_1, content_1, card_id_2, content_2)
        {
            var i = card_id_1 - 1, j = card_id_2 - 1;

            $scope.playground[i] = undefined;// content_1;
            $scope.playground[j] = undefined;// content_2;
            $scope.stats[player].turns += 1;
            $scope.stats[player].picks += 1;

            clearTimeout(timeouts[i]);
            clearTimeout(timeouts[j]);

            //timeouts[i] = setTimeout(function() 
            //{
            //   $scope.playground[i] = undefined;
            //    $scope.$apply();
            //}, 
            //TIMEOUT);

            //timeouts[j] = setTimeout(function() 
            //{
            //    $scope.playground[j] = undefined;
            //    $scope.$apply();
            //}, 
            //TIMEOUT);

            $scope.$apply();
        };

        pexeso.onjoin = function(player)
        {
            $scope.stats[player] = { turns: 0, picks: 0 };
            $scope.$apply();
        };

        pexeso.onstop = function()
        {
            setTimeout(function() { document.location.href = '#/'; }, 3000);
        };

    	$scope.pexeso = pexeso;
    };

    $scope.shouldShowCard = function(card)
    {
        return typeof card !== 'undefined';
    };

    $scope.shouldShowContent = function(card)
    {
        return typeof card === 'string';
    };

    $scope.clicked = function(i)
    {
        $scope.pexeso.turnCard(i+1);
    };
}); 