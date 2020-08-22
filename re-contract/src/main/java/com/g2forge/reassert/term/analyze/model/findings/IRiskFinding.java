package com.g2forge.reassert.term.analyze.model.findings;

import org.slf4j.event.Level;

import com.g2forge.alexandria.java.core.enums.EnumException;
import com.g2forge.reassert.core.model.contract.TermRelation;
import com.g2forge.reassert.core.model.report.ITerminalFinding;
import com.g2forge.reassert.term.eee.explain.model.IExplained;

public interface IRiskFinding extends ITerminalFinding {
	@Override
	public default Level getLevel() {
		final TermRelation relation = getResult().get();
		switch (relation) {
			case Included:
				return Level.WARN;
			case Excluded:
				return Level.INFO;
			case Unspecified:
				return Level.ERROR;
			default:
				throw new EnumException(TermRelation.class, relation);
		}
	}

	public IExplained<TermRelation> getResult();
}
