var board = "_________";
var humanPlaying = false;
var size = Math.sqrt(board.length);

var setAt = function(s, index, character) {
        s = s.split('');
        s[index] = character;
        return s.join('');
}

var gotBoard = function(newBoard) {
    setPlaying(true);
    board = newBoard;
    updateBoard(board);
}

var clicked = function(index) {
    if (!humanPlaying || board[index] != '_') {
        return;
    }

    setPlaying(false);
    board = setAt(board, index, 'X');
    updateBoard(board);
    $.get('solve/O/' + board, gotBoard);
}

var buildBoard = function() {
    var tt = $('#tt');

    for (var i = 0; i < size; i++) {
        var row = $('<tr>')
        for (var j = 0; j < size; j++) {
            var cell = $('<td>');

            cell.html("foo");
            cell.attr('index', i * size + j);
            cell.attr('class', 'cell');

            cell.click(function() {
                clicked($(this).attr('index'));
            });

            cell.appendTo(row);
        }
        row.appendTo(tt);
    }
}

var setPlaying = function(playing) {
    info = $('#info')
    humanPlaying = playing;
    if (playing) {
        info.text('play now, human');
    } else {
        info.text('robot is thinking');
    }
}

var setWinner = function(player) {
    info = $('#info');
    humanPlaying = false;
    if (player == 'O') {
        info.text('robot wins');
    } else if (player == 'X') {
        info.text('human wins');
    } else {
        info.text('draw');
    }
}

var updateBoard = function(board) {
    $('.cell').each(function() {
        var cell = $(this);
        cell.text(board[cell.attr('index')]);
    });
    var extra = board.split(':');
    if (extra.length > 1) {
        setWinner(extra[1]);
    }
}

$(document).ready(function() {
    buildBoard();
    updateBoard(board);
    setPlaying(true);
});
