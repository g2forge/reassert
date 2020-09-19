package com.g2forge.reassert.express.explain;

import com.g2forge.reassert.express.explain.convert.ExplanationMode;
import com.g2forge.reassert.express.explain.convert.ExplanationRenderer;
import com.g2forge.reassert.express.explain.model.IExplained;

public class ATestExplanationRenderer {
	protected String render(ExplanationMode mode, final IExplained<?> explained) {
		return new ExplanationRenderer(mode).render(explained);
	}
}
