package com.g2forge.reassert.contract.model.finding;

import com.g2forge.reassert.core.model.contract.terms.TermRelation;
import com.g2forge.reassert.core.model.report.ITerminalFinding;
import com.g2forge.reassert.express.model.IExplained;

public interface IContractTermFinding extends ITerminalFinding {
	public IExplained<TermRelation> getResult();
}
