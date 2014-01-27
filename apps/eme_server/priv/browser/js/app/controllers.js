var emediaControllers = angular.module('emediaControllers', ['ui.bootstrap']);

emediaControllers.controller('RootCtrl', function ($scope, $http, $routeParams, $location) {
  $scope.name = "Root";
  $scope.go = function(loc) {
    $location.path(loc);
  };
});
emediaControllers.controller('AudioCtrl', function ($scope, $http, $routeParams) {
  $scope.name = "Audio";
});
emediaControllers.controller('PhotoCtrl', function ($scope, $http, $routeParams) {
  $scope.name = "Photo";
});
emediaControllers.controller('VideoCtrl', function ($scope, $http, $routeParams) {
  $scope.name = "Video : ";
  console.log($routeParams);
});
emediaControllers.controller('HelpCtrl', function ($scope, $http, $routeParams) {
  $scope.name = "Help";
});
