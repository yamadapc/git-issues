export const FETCH_ISSUES = 'FETCH_ISSUES';
export const FETCH_ISSUES_SUCCESS = 'FETCH_ISSUES_SUCCESS';

export function fetchIssues() {
  return ({fetch}) => {
    return {
      type: FETCH_ISSUES,
      payload: {
        promise: fetch('http://localhost:12000/api/issues')
          .then(res => res.json())
      }
    };
  };
}
