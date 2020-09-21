package com.g2forge.reassert.contract.model.findings.rule;

import com.g2forge.reassert.core.model.contract.terms.TermRelation;
import com.g2forge.reassert.core.model.report.ITerminalFinding;
import com.g2forge.reassert.express.explain.model.IExplained;

public interface IRuleFinding extends ITerminalFinding {
	public IExplained<TermRelation> getResult();
}
