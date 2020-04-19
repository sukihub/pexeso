'use strict';

pexeso.controller('GamesController', function GamesController($scope) {

    $scope.games = [];
    $scope.not_connected = true;
    $scope.new_game_name = '';

    Games.initialize(function()
    {
        $scope.not_connected = false;
        $scope.$apply();
    });

    Games.updated(function(games)
    {
        $scope.games = games;
        $scope.$apply();
    });
    
    $scope.refresh = function()
    {
        Games.refresh();
    };

    $scope.create = function()
    {
        Games.create($scope.new_game_name);
        $scope.new_game_name = '';
    };

}); 