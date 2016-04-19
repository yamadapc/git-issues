import * as actions from './actions';
import {Map, Record} from 'immutable';
import keyBy from 'lodash/keyBy';

const InitialState = Record({
  map: Map()
});
const initialState = new InitialState();

// Note how JSON from server is revived to immutable record.
const revive = ({map}) => initialState.merge({
  map: Map(map).map(issue => new Map(issue))
});

export default function(state = initialState, action) {
  if (!(state instanceof InitialState)) return revive(state);

  switch(action.type) {
    case actions.FETCH_ISSUES_SUCCESS: {
      const issues = action.payload.map(i => new Map(i));
      console.log(keyBy(issues, i => i.get('issueNumber')));
      return state.update('map', map => map.merge(keyBy(issues, i => i.get('issueNumber'))));
    }
  }

  return state;
}
