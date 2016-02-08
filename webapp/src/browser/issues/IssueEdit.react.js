import Component from 'react-pure-render/component';
import shortParagraph from '../../images/short-paragraph.png';
import React from 'react';
import {Link} from 'react-router';
import _ from 'lodash';
import fields from '../../common/components/fields';

class IssueForm extends Component {
  onSubmit(e) {
    e.preventDefault();
    console.log(e);
    // actions.updateIssue();
  }

  render() {
    debugger;
    const issue = this.props.model;
    const {fields} = this.props;
    const {title, body} = fields;

    return (
      <form onSubmit={this.onSubmit} className="ui form" action="">
        <div className="field">
          <input placeholder="Issue Title"
                 {...title} />
        </div>
        <div className="field">
          <textarea placeholder="Issue Body"
                    {...body} />
        </div>

        <div className="ui buttons">
          <Link className="ui button" to={'/issues/' + issue.get('issueNumber')}>Cancel</Link>
          <div className="or"></div>
          <button className="ui positive button" type="submit">Save</button>
        </div>
      </form>
    );
  }
}

class IssueEdit extends Component {
  render() {
    const issue = this.props.issues.map.get(this.props.params.number);

    return (
      <div className="ui container">
        <div className="ui huge breadcrumb" style={{margin: '20px auto'}}>
          <Link className="section" to='/'>git-issues</Link>
          <div className="divider"> / </div>
          <Link className="section" to='/'>issues</Link>
          <div className="divider"> / </div>
          <Link className="active section" to={'/issues/' + issue.get('issueNumber')}>
            {issue.get('issueNumber')}
          </Link>
        </div>

        <IssueForm model={issue}
                   {...this.props} />
      </div>
    );
  }
}

IssueEdit = fields(IssueEdit, {
  path: 'issue',
  fields: [
    'title',
    'body'
  ]
});

export default IssueEdit;
