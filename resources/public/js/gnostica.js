"use strict";

function sendMessage() {
  var inputElement = document.getElementById('messageInput');
  var message = inputElement.value;
  websocket.send(message);
  inputElement.value = '';
}

function makeClientMessage(msg) {
  var template = $('#message').html();
  Mustache.parse(template);   // optional, speeds up future uses
  var context = {message: msg};
  var rendered = Mustache.render(template, context);
  return rendered;
}

function appendMessage(msg) {
  $('#messages').append(msg);
}

function scrollToLastMessage() {
  var messages = document.getElementById("messages");
  messages.scrollTop = messages.scrollHeight;
}

function handleMessage(msg) {
  appendMessage(makeClientMessage(msg.data));
  scrollToLastMessage();
}

function handleError(error) {
  appendMessage(makeClientMessage("Error!"));
  console.log('WebSocket Error ' + error);
}

function handleConnectionOpened() {
  appendMessage(makeClientMessage('Connected!'));
}

window.onload = function() {
  appendMessage(makeClientMessage('Connecting to server ...'));
  window.websocket = new WebSocket('ws://localhost:3000/ws');

  websocket.onopen = handleConnectionOpened;
  websocket.onerror = handleError;
  websocket.onmessage = handleMessage;
};
