package com.g2forge.reassert.reassert.model.finding;

import org.slf4j.event.Level;

import com.g2forge.reassert.contract.model.findings.IRiskFinding;
import com.g2forge.reassert.core.model.contract.terms.TermRelation;
import com.g2forge.reassert.expression.explain.model.IExplained;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;

@Data
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class TestRiskFinding implements IRiskFinding {
	protected final Level level;

	protected final String description;

	@Override
	public IExplained<TermRelation> getResult() {
		throw new UnsupportedOperationException();
	}
}
