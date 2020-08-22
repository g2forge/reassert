package com.g2forge.reassert.contract.analyze.model.findings;

import org.slf4j.event.Level;

import com.g2forge.alexandria.java.core.enums.EnumException;
import com.g2forge.reassert.contract.analyze.model.findings.IRiskFinding;
import com.g2forge.reassert.contract.eee.explain.model.IExplained;
import com.g2forge.reassert.core.model.contract.TermRelation;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;

@Data
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class ConditionFinding implements IRiskFinding {
	protected final IExplained<TermRelation> result;

	@Override
	public Level getLevel() {
		final TermRelation relation = getResult().get();
		switch (relation) {
			case Included:
				return Level.INFO;
			case Excluded:
				return Level.ERROR;
			case Unspecified:
				return Level.ERROR;
			default:
				throw new EnumException(TermRelation.class, relation);
		}
	}

	public boolean isSatisfied() {
		return getResult().get() == TermRelation.Included;
	}
}
