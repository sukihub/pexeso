'use strict';

var pexeso = angular.module('pexeso', []);

pexeso.config(function($routeProvider) {

  $routeProvider.
      when('/', {
        controller: 'GamesController',
        templateUrl: '/static/views/games.html'
      }).
      when('/pexeso/:game_name', {
      	controller: 'PexesoController',
      	templateUrl: '/static/views/pexeso.html'
      });
      
}); 