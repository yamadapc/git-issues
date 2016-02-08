import Component from 'react-pure-render/component';
import shortParagraph from '../../images/short-paragraph.png';
import React from 'react';
import {Link} from 'react-router';
import './Issue.css';

class IssueDescription extends Component {
  render() {
    if(this.props.body.length === 0) {
      return (
        <div className="empty-description">
          <i className="warning sign icon"></i>
          <h1>Empty issue description!</h1>
          <Link to={'/issues/' + this.props.number + '/edit'}>Click here to edit it</Link>
        </div>
      );
    }

    return (
      <p>
        {this.props.body}
      </p>
    );
  }
}

export default class Issue extends Component {
  render() {
    const issue = this.props.issues.map.get(this.props.params.number);

    return (
      <div className="ui container">
        <h1>{issue.get('issueTitle')}</h1>
        <IssueDescription body={issue.get('issueBody')}
                          number={issue.get('issueNumber')} />
      </div>
    );
  }
}
