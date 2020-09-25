package com.g2forge.reassert.contract.model.finding;

import org.slf4j.event.Level;

import com.g2forge.alexandria.java.core.enums.EnumException;
import com.g2forge.reassert.core.model.contract.terms.TermRelation;
import com.g2forge.reassert.core.model.report.ITerminalFinding;

public interface INoticeFinding extends IContractTermFinding, ITerminalFinding {
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
}
