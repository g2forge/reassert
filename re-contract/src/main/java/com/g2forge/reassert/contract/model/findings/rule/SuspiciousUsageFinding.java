package com.g2forge.reassert.contract.model.findings.rule;

import com.g2forge.alexandria.java.adt.name.IStringDescribed;
import com.g2forge.alexandria.java.function.IFunction1;
import com.g2forge.reassert.core.model.contract.terms.TermRelation;
import com.g2forge.reassert.express.explain.model.IExplained;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;

@Data
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class SuspiciousUsageFinding implements INoticeFinding {
	public interface ITermsAttribute extends IStringDescribed, IFunction1<IExplained<TermRelation>, SuspiciousUsageFinding> {
		@Override
		public default SuspiciousUsageFinding apply(IExplained<TermRelation> t) {
			return new SuspiciousUsageFinding(t, this);
		}
	}

	protected final IExplained<TermRelation> result;

	protected final ITermsAttribute attribute;
}
