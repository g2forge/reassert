package com.g2forge.reassert.contract.v2.model.finding.rule;

import com.g2forge.reassert.core.model.contract.terms.TermRelation;
import com.g2forge.reassert.core.model.report.ITerminalFinding;
import com.g2forge.reassert.express.v2.model.IExplained;

public interface IRuleFinding extends ITerminalFinding {
	public IExplained<TermRelation> getResult();
}
