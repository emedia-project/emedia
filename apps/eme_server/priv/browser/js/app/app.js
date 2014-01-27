var emediaApp = angular.module('emediaApp', [
    'ui.bootstrap',
    'ngRoute',
    'emediaControllers'
]).run(function($rootScope, $location, $http, $modal) {
  if($location.path() == '') { $location.path('/'); }
  $rootScope.location = $location;

  $rootScope.about = function() {
    var modalInstance = $modal.open({
      templateUrl: '/browser/modal/about.html',
      controller: ModalInstanceCtrl
    });
  };
});

emediaApp.config(['$routeProvider',
  function($routeProvider) {
    $routeProvider.
    when('/', {
      templateUrl: '/browser/partial/root.html',
      controller: 'RootCtrl'
    }).
    when('/audio', {
      templateUrl: '/browser/partial/audio.html',
      controller: 'AudioCtrl'
    }).
    when('/photo', {
      templateUrl: '/browser/partial/photo.html',
      controller: 'PhotoCtrl'
    }).
    when('/video', {
      templateUrl: '/browser/partial/video.html',
      controller: 'VideoCtrl'
    }).
    when('/help', {
      templateUrl: '/browser/partial/help.html',
      controller: 'HelpCtrl'
    }).
    otherwise({
      redirectTo: '/'
    });
  }
]);

var ModalInstanceCtrl = function ($scope, $modalInstance) {
  $scope.ok = function () {
    $modalInstance.close();
  };
};
