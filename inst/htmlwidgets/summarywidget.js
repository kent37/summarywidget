HTMLWidgets.widget({

  name: 'summarywidget',

  type: 'output',

  factory: function(el, width, height) {

    return {

      renderValue: function(x) {
        var value = 0;
        switch (x.settings.statistic) {
          case 'count':
            value = x.data.length;
            break;
          case 'sum':
            value = x.data.reduce(function(acc, val) {return acc + val;}, 0);
            break;
          case 'mean':
            value = x.data.reduce(function(acc, val) {return acc + val;}, 0) / x.data.length;
            break;
        }

        if (x.settings.digits !== null) value = value.toFixed(x.settings.digits);
        el.innerText = value;

      },

      resize: function(width, height) {

        // TODO: code to re-render the widget with a new size

      }

    };
  }
});
