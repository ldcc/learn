import React from 'react';

const h = require('react-hyperscript');
const render = require('react-dom').render;
const Component = require('react').Component;
const ReactTooltip = require('react-tooltip-component');

class Tooltip extends Component {

  render() {
    return h(ReactTooltip, {
      position: 'top',
      title: 'Tooltip',
    });
  };

}

window.onload = function() {
  render(
      h(Tooltip),
      document.getElementById('root'),
  );
};
