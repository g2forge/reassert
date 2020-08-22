package com.g2forge.reassert.term.eee.explain;

import com.g2forge.reassert.term.eee.explain.convert.ExplanationMode;
import com.g2forge.reassert.term.eee.explain.convert.ExplanationRenderer;
import com.g2forge.reassert.term.eee.explain.model.IExplained;

public class ATestExplanationRenderer {
	protected String render(ExplanationMode mode, final IExplained<?> explained) {
		return new ExplanationRenderer(mode).render(explained);
	}
}
