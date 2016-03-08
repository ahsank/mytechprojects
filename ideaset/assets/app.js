app = angular.module('ideaApp', ['ngMaterial', 'controllerModule']);

angular.module('controllerModule', [])
    .controller(
        'IdeaController',
        ['$scope', '$mdSidenav', function($scope, $mdSidenav) {
	    $scope.notes = [];

            $scope.toggleSidenav = function () {
                $mdSidenav('left').open();
            }

	    $scope.createNote = function() {
		var note = {
		    id: new Date().getTime(),
		    title: 'New Note',
		    body: 'Double click to edit'
		};

		$scope.notes.push(note);
	    };

	    $scope.deleteNote = function(id) {
		$scope.handleDeletedNoted(id);
	    };

	    $scope.handleDeletedNoted = function(id) {
		var oldNotes = $scope.notes,
		newNotes = [];

		angular.forEach(oldNotes, function(note) {
		    if(note.id !== id) newNotes.push(note);
		});

		$scope.notes = newNotes;
	    }
        }]);

app.directive('myDraggable', ['$document', function($document) {
  return {
    link: function(scope, element, attr) {
      var startX = 0, startY = 0, x = 0, y = 0;

      element.css({
       position: 'relative',
       cursor: 'pointer'
      });

      element.on('mousedown', function(event) {
        // Prevent default dragging of selected content
        event.preventDefault();
        startX = event.pageX - x;
        startY = event.pageY - y;
        $document.on('mousemove', mousemove);
        $document.on('mouseup', mouseup);
      });

      function mousemove(event) {
        y = event.pageY - startY;
        x = event.pageX - startX;
        element.css({
          top: y + 'px',
          left:  x + 'px'
        });
      }

      function mouseup() {
        $document.off('mousemove', mousemove);
        $document.off('mouseup', mouseup);
      }
    }
  };
}]);

app.directive('focusMe', function($timeout) {
  return {
    link: function(scope, element, attrs) {
      scope.$watch(attrs.focusMe, function(value) {
        if(value === true) { 
          console.log('value=',value);
          //$timeout(function() {
            element[0].focus();
            scope[attrs.focusMe] = false;
          //});
        }
      });
    }
  };
})
