import Component from 'react-pure-render/component';
import Helmet from 'react-helmet';
import React, {PropTypes} from 'react';
import {FormattedHTMLMessage} from 'react-intl';
import {Link} from 'react-router';
import '../../vendor/semantic/dist/semantic.css';
import './Page.css';
import shortParagraph from '../../images/short-paragraph.png';

export default class Page extends Component {
  static propTypes = {
    // Why not PropTypes.object.isRequired? Because:
    // https://github.com/rackt/react-router/issues/1505
    msg: PropTypes.object
  };

  render() {
    const {msg: {home: msg}} = this.props;
    const issueList = this.props.issues.map.map(function(issue, id) {
      const {issueNumber, issueTitle, issueBody} =
              issue.toObject ? issue.toObject() : issue;
      return (
        <div className="item" key={id}>
          <div className="content">
            <Link className="header" to={'/issues/' + issueNumber}>{issueTitle}</Link>

            <div className="meta">
              <span>Description</span>
            </div>
            <div className="description">
              <p>
                <img width="425" alt="" src={shortParagraph} />
                {issueBody}
              </p>
            </div>
          </div>
        </div>
      );
    }).toList();

    return (
      <div className="ui container">
        <Helmet title={msg.title} />
        <h1>git-issues</h1>

        <div className="ui grid">
          <div className="ten wide column">
            <h2>Issues</h2>
            <div className="ui items">
              {issueList}
            </div>
          </div>

          <div className="six wide column">
            <h2>Feed</h2>
          </div>
        </div>
      </div>
    );
  }
}
